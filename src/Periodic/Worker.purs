module Periodic.Worker
  ( WK
  , WorkerT
  , runWorkerT
  , addFunc
  , work

  , Job
  , JobT
  , done
  , fail
  , schedLater
  , funcName
  , name
  , workload
  , module Periodic.Types
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Data.Newtype (class Newtype)
import Control.Monad.Reader (ReaderT, runReaderT)
import Periodic.Types (PERIODIC)

foreign import data Job :: Type
foreign import data Worker :: Type
foreign import newWorker :: forall a eff.  a -> Eff eff Worker
foreign import _work :: forall eff.  Worker -> Int -> Eff eff Unit
foreign import _addFunc :: forall eff. Worker -> String -> (Job -> Eff eff Unit) -> Eff eff Unit

foreign import _done :: forall eff. Job -> Eff eff Unit
foreign import _fail :: forall eff. Job -> Eff eff Unit
foreign import _schedLater :: forall eff. Job -> Int -> Eff eff Unit

foreign import _funcName :: forall eff. Job -> Eff eff String
foreign import _name :: forall eff. Job -> Eff eff String
foreign import _workload :: forall eff. Job -> Eff eff String

data WK eff m = WK (m Unit -> Eff (periodic :: PERIODIC | eff) Unit) Worker

newtype WorkerT eff m a = WorkerT (ReaderT (WK eff m) m a)
newtype JobT m a = JobT (ReaderT Job m a)

runWorkerT
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
   => (m Unit -> Eff (periodic :: PERIODIC | eff) Unit)
   -> a -> WorkerT eff m Unit -> m Unit
runWorkerT runEff a (WorkerT m) = do
  w <- liftEff $ newWorker a
  runReaderT m (WK runEff w)

runJobT
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> JobT m a -> m a
runJobT a (JobT m) = runReaderT m a

derive instance newtypeWorkerT :: Newtype (WorkerT eff m a) _

instance functorWorkerT :: Functor m => Functor (WorkerT eff m) where
  map f (WorkerT m) = WorkerT $ map f m

instance applyWorkerT :: Monad m => Apply (WorkerT eff m) where
  apply = ap

instance applicativeWorkerT :: Monad m => Applicative (WorkerT eff m) where
  pure = WorkerT <<< pure

instance bindWorkerT :: Monad m => Bind (WorkerT eff m) where
  bind (WorkerT m) k = WorkerT $ do
    a <- m
    case k a of
      WorkerT b -> b

instance monadWorkerT :: Monad m => Monad (WorkerT eff m)

instance monadTransWorkerT :: MonadTrans (WorkerT eff) where
  lift = WorkerT <<< lift

instance monadEffWorkerT
  :: MonadEff (periodic :: PERIODIC | eff) m
  => MonadEff (periodic :: PERIODIC | eff) (WorkerT eff m) where
  liftEff = lift <<< liftEff

instance monadAffWorkerT
  :: MonadAff (periodic :: PERIODIC | eff) m
  => MonadAff (periodic :: PERIODIC | eff) (WorkerT eff m) where
  liftAff = lift <<< liftAff

instance monadAskWorkerT :: Monad m => MonadAsk (WK eff m) (WorkerT eff m) where
  ask = WorkerT $ ask

derive instance newtypeJobT :: Newtype (JobT m a) _

instance functorJobT :: Functor m => Functor (JobT m) where
  map f (JobT m) = JobT $ map f m

instance applyJobT :: Monad m => Apply (JobT m) where
  apply = ap

instance applicativeJobT :: Monad m => Applicative (JobT m) where
  pure = JobT <<< pure

instance bindJobT :: Monad m => Bind (JobT m) where
  bind (JobT m) k = JobT $ do
    a <- m
    case k a of
      JobT b -> b

instance monadJobT :: Monad m => Monad (JobT m)

instance monadTransJobT :: MonadTrans JobT where
  lift = JobT <<< lift

instance monadEffJobT :: MonadEff eff m => MonadEff eff (JobT m) where
  liftEff = lift <<< liftEff

instance monadAffJobT :: MonadAff eff m => MonadAff eff (JobT m) where
  liftAff = lift <<< liftAff

instance monadAskJobT :: Monad m => MonadAsk Job (JobT m) where
  ask = JobT $ ask

addFunc
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => String -> JobT m Unit -> WorkerT eff m Unit
addFunc func m = do
  (WK runEff w) <- ask
  liftEff $ _addFunc w func $ \job ->
    runEff $ runJobT job m

work
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Int -> WorkerT eff m Unit
work size = do
  (WK _ w) <- ask
  liftEff $ _work w size

done
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m Unit
done = liftEff <<< _done =<< ask

fail
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m Unit
fail = liftEff <<< _fail =<< ask

schedLater
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  =>  Int -> JobT m Unit
schedLater delay = liftEff <<< flip _schedLater delay =<< ask

funcName
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m String
funcName = liftEff <<< _funcName =<< ask

name
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m String
name = liftEff <<< _name =<< ask

workload
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m String
workload = liftEff <<< _workload =<< ask
