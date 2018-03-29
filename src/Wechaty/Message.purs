module Wechaty.Message
  ( say
  , sayTo
  , content
  , from
  , self
  , room
  , handleContact
  , handleRoom
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Wechaty.Types (Contact, ContactM, Message, MessageM, Room, RoomM, runContactM, runRoomM)

foreign import _say :: forall a eff. Fn2 Message a (Eff eff (Promise Unit))
foreign import _sayTo :: forall a eff. Fn3 Message Contact a (Eff eff (Promise Unit))
foreign import _getContent :: forall eff. Message -> Eff eff String
foreign import _getFrom :: forall eff. Message -> Eff eff Contact
foreign import _getSelf :: forall eff. Message -> Eff eff Boolean
foreign import _room :: forall eff. Fn3 (Room -> Maybe Room) (Maybe Room) Message (Eff eff (Maybe Room))

runSay :: forall a eff. Message -> a -> Aff eff Unit
runSay msg a = liftEff (runFn2 _say msg a) >>= toAff

say :: forall a eff. a -> MessageM eff Unit
say a = do
  msg <- ask
  lift $ runSay msg a

runSayTo :: forall a eff. Message -> Contact -> a -> Aff eff Unit
runSayTo msg contact a = liftEff (runFn3 _sayTo msg contact a) >>= toAff

sayTo :: forall a eff. Contact -> a -> MessageM eff Unit
sayTo contact a = do
  msg <- ask
  lift $ runSayTo msg contact a

content :: forall eff. MessageM eff String
content = do
  msg <- ask
  liftEff $ _getContent msg

from :: forall eff. MessageM eff Contact
from = do
  msg <- ask
  liftEff $ _getFrom msg

self :: forall eff. MessageM eff Boolean
self = do
  msg <- ask
  liftEff $ _getSelf msg

room :: forall eff. MessageM eff (Maybe Room)
room = do
  msg <- ask
  liftEff $ runFn3 _room Just Nothing msg

handleRoom :: forall eff. Room -> (Contact -> Boolean -> String -> RoomM eff Unit) -> MessageM eff Unit
handleRoom r m = do
  msg <- content
  s <- self
  f <- from
  lift $ runRoomM r (m f s msg)

handleContact :: forall eff. (String -> ContactM eff Unit) -> MessageM eff Unit
handleContact m = do
  msg <- content
  f <- from
  lift $ runContactM f (m msg)
