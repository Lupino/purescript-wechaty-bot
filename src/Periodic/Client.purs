module Periodic.Client
  ( Client
  , newClient
  , submitJob
  , ping
  , status
  , dropFunc
  , removeJob
  , module Periodic.Types
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn2, runFn2)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Compat (fromEffFnAff, EffFnAff)
import Periodic.Types (PERIODIC)

foreign import data Client :: Type
foreign import _newClient :: forall a b eff. Fn2 a b (Eff eff Client)
foreign import _submitJob :: forall a eff. Client -> a -> EffFnAff eff Unit
foreign import _ping :: forall eff. Client -> EffFnAff eff Boolean
foreign import _status :: forall a eff. Client -> EffFnAff eff a
foreign import _dropFunc :: forall eff. Client -> String -> EffFnAff eff Unit
foreign import _removeJob :: forall a eff. Client -> a -> EffFnAff eff Unit

newClient :: forall a b eff. a -> b -> Eff (periodic :: PERIODIC | eff) Client
newClient a b = runFn2 _newClient a b

submitJob :: forall a eff. Client -> a -> Aff (periodic :: PERIODIC | eff) Unit
submitJob c = fromEffFnAff <<< _submitJob c

ping :: forall eff. Client -> Aff (periodic ::PERIODIC | eff) Boolean
ping = fromEffFnAff <<< _ping

status :: forall a eff. Client -> Aff (periodic ::PERIODIC | eff) a
status = fromEffFnAff <<< _status

dropFunc :: forall eff. Client -> String -> Aff (periodic ::PERIODIC | eff) Unit
dropFunc c = fromEffFnAff <<< _dropFunc c

removeJob :: forall a eff. Client -> a -> Aff (periodic ::PERIODIC | eff) Unit
removeJob c = fromEffFnAff <<< _removeJob c
