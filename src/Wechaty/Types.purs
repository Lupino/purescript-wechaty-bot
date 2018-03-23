module Wechaty.Types
  ( WECHATY
  , Wechaty
  , WechatyM
  , runWechatyM

  , Contact
  , ContactM
  , runContactM

  , Message
  , MessageM
  , runMessageM

  , Room
  , RoomM
  , runRoomM
  ) where

import Prelude
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (kind Effect)

foreign import data WECHATY :: Effect
foreign import data Wechaty :: Type
type WechatyM eff = ReaderT Wechaty (Aff (wechaty :: WECHATY | eff))

runWechatyM :: forall a eff. Wechaty -> WechatyM eff a -> Aff (wechaty :: WECHATY | eff) a
runWechatyM wechaty = flip runReaderT wechaty

foreign import data Contact :: Type
type ContactM eff = ReaderT Contact (Aff (wechaty :: WECHATY | eff))

runContactM :: forall a eff. Contact -> ContactM eff a -> Aff (wechaty :: WECHATY | eff) a
runContactM contact = flip runReaderT contact

foreign import data Message :: Type
type MessageM eff = ReaderT Message (Aff (wechaty :: WECHATY | eff))

runMessageM :: forall a eff. Message -> MessageM eff a -> Aff (wechaty :: WECHATY | eff) a
runMessageM msg = flip runReaderT msg

foreign import data Room :: Type
type RoomM eff = ReaderT Room (Aff (wechaty :: WECHATY | eff))

runRoomM :: forall a eff. Room -> RoomM eff a -> Aff (wechaty :: WECHATY | eff) a
runRoomM room = flip runReaderT room
