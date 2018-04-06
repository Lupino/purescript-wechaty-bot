module Plan.Trans
  ( Param (..)
  , ActionT
  , params
  , param
  , Pattern (..)
  , regexPattern
  , regexPattern_
  , paramPattern
  , RouteRef
  , initRouteRef
  , PlanT
  , runPlanT
  , respond
  , reply
  ) where

import Prelude
import Control.Monad.State.Trans (StateT, evalStateT, class MonadState, state, get)
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..), fromRight)
import Data.Newtype (class Newtype)
import Data.Array (head, tail, mapWithIndex, catMaybes, zipWith, concat)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Partial.Unsafe (unsafePartial)
import Data.String.Regex (Regex, match, regex, replace)
import Data.String.Regex.Flags (noFlags, global)
import Data.String (takeWhile, drop)

data Param = Param String String

derive instance eqParam :: Eq Param
derive instance ordParam :: Ord Param

newtype ActionT m a = ActionT (ExceptT Error (ReaderT (Array Param) m) a)

runActionT :: forall m a. Array Param -> ActionT m a -> m (Either Error a)
runActionT p (ActionT m)= flip runReaderT p $ runExceptT m

derive instance newtypeActionT :: Newtype (ActionT m a) _

instance functorActionT :: Functor m => Functor (ActionT m) where
  map f (ActionT m) = ActionT $ map f m

instance applyActionT :: Monad m => Apply (ActionT m) where
  apply = ap

instance applicativeActionT :: Monad m => Applicative (ActionT m) where
  pure = ActionT <<< pure

instance bindActionT :: Monad m => Bind (ActionT m) where
  bind (ActionT m) k = ActionT $ do
    a <- m
    case k a of
      ActionT b -> b

instance monadActionT :: Monad m => Monad (ActionT m)

instance monadTransActionT :: MonadTrans ActionT where
  lift = ActionT <<< lift <<< lift

instance monadEffActionT :: MonadEff eff m => MonadEff eff (ActionT m) where
  liftEff = lift <<< liftEff

instance monadAffActionT :: MonadAff eff m => MonadAff eff (ActionT m) where
  liftAff = lift <<< liftAff

params :: forall m. Monad m => ActionT m (Array Param)
params = ActionT $ ask

param :: forall m. Monad m => String -> ActionT m String
param k = ActionT $ do
  xs <- ask
  except $ go (head xs) (tail xs)
  where go :: Maybe Param -> Maybe (Array Param) -> Either Error String
        go Nothing _ = leftErr
        go (Just (Param k0 v)) Nothing = if k0 == k then Right v else leftErr
        go (Just (Param k0 v)) (Just xs)
          | k0 == k = Right v
          | otherwise = go (head xs) (tail xs)

        leftErr = Left $ error $ "param: " <> k <> " is required"

newtype Pattern = Pattern (String -> Maybe (Array Param))

derive instance newtypePattern :: Newtype Pattern _

regexPattern_ :: Regex -> Pattern
regexPattern_ reg = Pattern go
  where go :: String -> Maybe (Array Param)
        go xs = do
          m <- match reg xs
          pure $ mapWithIndex toParam $ catMaybes m
          where toParam :: Int -> String -> Param
                toParam idx v = Param (show idx) v

regexPattern :: String -> Pattern
regexPattern = unsafePartial $ fromRight <<< map regexPattern_ <<< flip regex noFlags

reSpecParam :: Regex
reSpecParam = unsafePartial $ fromRight $ regex ":[^:]+:" global

paramPattern :: String -> Pattern
paramPattern xs = Pattern go
  where reg = unsafePartial $ fromRight $ regex ("^" <> replace reSpecParam "(.+)" xs <> "$") noFlags
        keys = catMaybes <$> match reSpecParam xs

        go :: String -> Maybe (Array Param)
        go ys = do
          vs <- catMaybes <$> match reg ys
          ks <- keys
          vs' <- tail vs
          pure $ zipWith toParam ks vs'
          where toParam :: String -> String -> Param
                toParam k v = Param (takeWhile (_ /= ':') $ drop 1 k) v

data Route m a = Route Pattern (ActionT m a)

newtype RouteRef m a = RouteRef (Ref (Array (Route m a)))
derive instance newtypeRouteRef :: Newtype (RouteRef m a) _

initRouteRef :: forall r m a. Eff (ref :: REF | r) (RouteRef m a)
initRouteRef = map RouteRef $ newRef []

addRoute :: forall r m a. RouteRef m a -> Route m a -> Eff (ref :: REF | r) Unit
addRoute (RouteRef ref) x = modifyRef ref $ \xs -> x:xs

routes :: forall r m a. RouteRef m a -> Eff (ref :: REF | r) (Array (Route m a))
routes (RouteRef ref) = readRef ref

newtype PlanT a m b = PlanT (StateT (RouteRef m a) m b)

runPlanT :: forall a b m. Monad m => RouteRef m a -> PlanT a m b -> m b
runPlanT ref (PlanT m) = evalStateT m ref

derive instance newtypePlanT :: Newtype (PlanT a m b) _

instance functorPlanT :: Functor m => Functor (PlanT a m) where
  map f (PlanT m) = PlanT $ map f m

instance applyPlanT :: Monad m => Apply (PlanT a m) where
  apply = ap

instance applicativePlanT :: Monad m => Applicative (PlanT a m) where
  pure = PlanT <<< pure

instance bindPlanT :: Monad m => Bind (PlanT a m) where
  bind (PlanT m) k = PlanT $ do
    a <- m
    case k a of
      PlanT b -> b

instance monadPlanT :: Monad m => Monad (PlanT a m)

instance monadTransPlanT :: MonadTrans (PlanT a) where
  lift = PlanT <<< lift

instance monadEffPlanT :: MonadEff eff m => MonadEff eff (PlanT a m) where
  liftEff = lift <<< liftEff

instance monadAffPlanT :: MonadAff eff m => MonadAff eff (PlanT a m) where
  liftAff = lift <<< liftAff

instance monadStatePlanT :: Monad m => MonadState (RouteRef m a) (PlanT a m) where
  state f = PlanT $ state f

respond :: forall a m r. MonadEff (ref :: REF | r) m => Pattern -> ActionT m a -> PlanT a m Unit
respond pat action = liftEff <<< flip addRoute (Route pat action) =<< get

data MatchRoute m a = MatchRoute (Array Param) (ActionT m a)

matchRoute :: forall m a. String -> Array (Route m a) -> Maybe (MatchRoute m a)
matchRoute xs rs = go (head rs) (tail rs)
  where go :: forall m0 a0. Maybe (Route m0 a0) -> Maybe (Array (Route m0 a0)) -> Maybe (MatchRoute m0 a0)
        go Nothing _ = Nothing
        go (Just (Route (Pattern f) m)) ys =
          case f xs of
            Just p -> Just (MatchRoute p m)
            Nothing ->
              case ys of
                Nothing -> Nothing
                Just ys' -> go (head ys') (tail ys')

reply :: forall a m r. MonadEff (ref :: REF | r) m => String -> PlanT a m (Either Error a)
reply xs = do
  ref <- get
  rs <- liftEff $ routes ref
  case matchRoute xs rs of
    Nothing -> pure $ Left $ error "route not found."
    Just (MatchRoute ps m) -> lift $ runActionT ps m
