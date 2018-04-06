module Plan.Trans where

import Prelude
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask)
import Control.Monad.Except.Trans (ExceptT, runExceptT, except)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Newtype (class Newtype)
import Data.Array (head, tail)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Class (class MonadEff, liftEff)

data Param = Param String String

derive instance eqParam :: Eq Param
derive instance ordParam :: Ord Param

newtype ActionT m a = ActionT (ExceptT Error (ReaderT (Array Param) m) a)
derive instance newtypeActionT :: Newtype (ActionT m a) _

runActionT :: forall m a. Array Param -> ActionT m a -> m (Either Error a)
runActionT p (ActionT m)= flip runReaderT p $ runExceptT m

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

params :: forall m. Monad m => ActionT m (Array Param)
params = ActionT $ ask

param :: forall m. Monad m => String -> ActionT m String
param k = ActionT $ do
  xs <- ask
  except $ go (head xs) (tail xs)
  where go :: Maybe Param -> Maybe (Array Param) -> Either Error String
        go Nothing _ = Left $ error $ "param: " <> k <> "is required"
        go _ Nothing = Left $ error $ "param: " <> k <> "is required"
        go (Just (Param k0 v)) (Just xs)
          | k0 == k = Right v
          | otherwise = go (head xs) (tail xs)
