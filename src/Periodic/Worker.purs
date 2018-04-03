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
import Control.Monad.Reader (ReaderT, runReaderT, ask)
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

type WorkerT eff m = ReaderT (WK eff m) m
type JobT m = ReaderT Job m

runWorkerT
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
   => (m Unit -> Eff (periodic :: PERIODIC | eff) Unit) -> a -> WorkerT eff m Unit -> Eff (periodic :: PERIODIC | eff) Unit
runWorkerT runEff a m = do
  w <- newWorker a
  runEff $ runReaderT m (WK runEff w)

runJobT
  :: forall a m eff. MonadEff (periodic :: PERIODIC | eff) m
  => Job -> JobT m a -> m a
runJobT a = flip runReaderT a

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
