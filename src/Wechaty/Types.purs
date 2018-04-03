module Wechaty.Types
  ( WECHATY

  , Room
  , RoomM
  , runRoomM
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (kind Effect)

foreign import data WECHATY :: Effect

foreign import data Room :: Type
type RoomM eff = ReaderT Room (Aff (wechaty :: WECHATY | eff))

runRoomM :: forall a eff. Room -> RoomM eff a -> Aff (wechaty :: WECHATY | eff) a
runRoomM room = flip runReaderT room
