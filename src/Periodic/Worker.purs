module Periodic.Worker
  ( Worker
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

import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Periodic.Types (PERIODIC)

foreign import data Job :: Type
foreign import data Worker :: Type
foreign import newWorker
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
  => a -> m Worker
foreign import _work
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Worker -> Int -> m Unit
foreign import _addFunc
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Worker -> String -> (Job -> m Unit) -> m Unit

foreign import _done
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> m Unit
foreign import _fail
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> m Unit
foreign import _schedLater
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> Int -> m Unit

foreign import _funcName
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> m String
foreign import _name
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> m String
foreign import _workload
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> m String

type WorkerT m = ReaderT Worker m
type JobT m = ReaderT Job m

runWorkerT
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
  => a -> WorkerT m a -> m a
runWorkerT a m = do
  w <- newWorker a
  runReaderT m w

runJobT
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> JobT m a -> m a
runJobT a = flip runReaderT a

addFunc
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => String -> JobT m Unit -> WorkerT m Unit
addFunc func m = do
  w <- ask
  lift $ _addFunc w func $ flip runJobT m

work
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Int -> WorkerT m Unit
work size = do
  w <- ask
  lift $ _work w size

done
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m Unit
done = lift <<< _done =<< ask

fail
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m Unit
fail = lift <<< _fail =<< ask

schedLater
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  =>  Int -> JobT m Unit
schedLater delay = lift <<< flip _schedLater delay =<< ask

funcName
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m String
funcName = lift <<< _funcName =<< ask

name
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m String
name = lift <<< _name =<< ask

workload
  :: forall m eff. MonadEff (periodic :: PERIODIC | eff) m
  => JobT m String
workload = lift <<< _workload =<< ask
