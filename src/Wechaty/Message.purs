module Wechaty.Message
  ( say
  , sayTo
  , content
  , from
  , self
  , room
  , handleContact
  , handleRoom
  , Message
  , MessageT
  , runMessageT
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Wechaty.Types (Contact, ContactM, Room, RoomM, WECHATY, runContactM, runRoomM)

foreign import data Message :: Type
type MessageT m = ReaderT Message m

runMessageT :: forall a m. Message -> MessageT m a -> m a
runMessageT msg = flip runReaderT msg


foreign import _say :: forall a eff. Fn2 Message a (Eff eff (Promise Unit))
foreign import _sayTo :: forall a eff. Fn3 Message Contact a (Eff eff (Promise Unit))
foreign import _getContent :: forall eff. Message -> Eff eff String
foreign import _getFrom :: forall eff. Message -> Eff eff Contact
foreign import _getSelf :: forall eff. Message -> Eff eff Boolean
foreign import _room :: forall eff. Fn3 (Room -> Maybe Room) (Maybe Room) Message (Eff eff (Maybe Room))

runSay :: forall a eff. Message -> a -> Aff eff Unit
runSay msg a = liftEff (runFn2 _say msg a) >>= toAff

say
  :: forall a m eff. MonadAff (wechaty :: WECHATY | eff) m
  => a -> MessageT m Unit
say a = do
  msg <- ask
  liftAff $ runSay msg a

runSayTo :: forall a eff. Message -> Contact -> a -> Aff eff Unit
runSayTo msg contact a = liftEff (runFn3 _sayTo msg contact a) >>= toAff

sayTo
  :: forall a m eff. MonadAff (wechaty :: WECHATY | eff) m
  => Contact -> a -> MessageT m Unit
sayTo contact a = do
  msg <- ask
  liftAff $ runSayTo msg contact a

content
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MessageT m String
content = do
  msg <- ask
  liftEff $ _getContent msg

from
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MessageT m Contact
from = do
  msg <- ask
  liftEff $ _getFrom msg

self
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MessageT m Boolean
self = do
  msg <- ask
  liftEff $ _getSelf msg

room
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MessageT m (Maybe Room)
room = do
  msg <- ask
  liftEff $ runFn3 _room Just Nothing msg

handleRoom
  :: forall m eff. MonadAff (wechaty :: WECHATY | eff) m
  => Room -> Boolean
  -> (Contact -> Boolean -> String -> RoomM eff Unit)
  -> MessageT m Unit
handleRoom r manager m = do
  msg <- content
  f <- from
  liftAff $ runRoomM r (m f manager msg)

handleContact
  :: forall m eff. MonadAff (wechaty :: WECHATY | eff) m
  => (String -> ContactM eff Unit) -> MessageT m Unit
handleContact m = do
  msg <- content
  f <- from
  liftAff $ runContactM f (m msg)
