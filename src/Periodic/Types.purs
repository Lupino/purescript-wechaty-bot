module Periodic.Types
  ( PERIODIC
  ) where

import Control.Monad.Eff (kind Effect)
foreign import data PERIODIC :: Effect
