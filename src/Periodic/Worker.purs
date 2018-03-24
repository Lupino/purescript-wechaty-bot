module Periodic.Worker
  ( Worker
  , WorkerM
  , runWorker
  , addFunc

  , Job
  , JobM
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
import Periodic.Types (PERIODIC)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)

foreign import data Job :: Type
foreign import data Worker :: Type
foreign import newWorker :: forall a eff. a -> Eff eff Worker
foreign import _work :: forall eff. Worker -> Int -> Eff eff Worker
foreign import _addFunc :: forall eff. Worker -> String -> (Job -> Eff eff Unit) -> Eff eff Unit

foreign import _done :: forall eff. Job -> Eff eff Unit
foreign import _fail :: forall eff. Job -> Eff eff Unit
foreign import _schedLater :: forall eff. Job -> Int -> Eff eff Unit

foreign import _funcName :: forall eff. Job -> Eff eff String
foreign import _name :: forall eff. Job -> Eff eff String
foreign import _workload :: forall eff. Job -> Eff eff String

type WorkerM eff = ReaderT Worker (Eff (periodic :: PERIODIC | eff))
type JobM eff = ReaderT Job (Eff (periodic :: PERIODIC | eff))

runWorker :: forall a eff. a -> WorkerM eff a -> Eff (periodic :: PERIODIC | eff) a
runWorker a m = do
  w <- newWorker a
  runReaderT m w

runJobM :: forall a eff. Job -> JobM eff a -> Eff (periodic :: PERIODIC | eff) a
runJobM a = flip runReaderT a

addFunc :: forall eff. String -> JobM eff Unit -> WorkerM eff Unit
addFunc func m = do
  w <- ask
  lift $ _addFunc w func $ flip runJobM m

done :: forall eff. JobM eff Unit
done = lift <<< _done =<< ask

fail :: forall eff. JobM eff Unit
fail = lift <<< _fail =<< ask

schedLater :: forall eff. Int -> JobM eff Unit
schedLater delay = lift <<< flip _schedLater delay =<< ask

funcName :: forall eff. JobM eff String
funcName = lift <<< _funcName =<< ask

name :: forall eff. JobM eff String
name = lift <<< _name =<< ask

workload :: forall eff. JobM eff String
workload = lift <<< _workload =<< ask
