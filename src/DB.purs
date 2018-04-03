module DB
  ( DB
  , formatTimeString
  , parseTimeString
  , class HasUser
  , getUser
  , setUser

  , Group (..)
  , mkGroup
  , saveGroup
  , getGroup
  , setGroupRepeat

  , Message (..)
  , message
  , setContent
  , setSchedAt

  , createMessage
  , updateMessage
  , getMessage
  , getMessageList
  , deleteMessage

  , subscribeMessage
  , unSubscribeMessage
  , getSubscribeList

  , roomSubscribeMessage
  , unRoomSubscribeMessage
  , getRoomSubscribeList
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe(..))
import Data.String (drop, null)

foreign import data DB :: Effect

foreign import formatTimeString :: Number -> String

foreign import readNumber :: String -> Number
foreign import startsWith :: String -> String -> Boolean

-- 1d 10h 10m 10s
parseTimeString_ :: String -> String -> Number
parseTimeString_ r n
  | startsWith n "d" = readNumber r * 24.0 * 60.0 * 60.0 + parseTimeString_ "" (drop 1 n)
  | startsWith n "h" = readNumber r * 60.0 * 60.0 + parseTimeString_ "" (drop 1 n)
  | startsWith n "m" = readNumber r * 60.0 + parseTimeString_ "" (drop 1 n)
  | startsWith n "s" = readNumber r + parseTimeString_ "" (drop 1 n)
  | startsWith n " " = parseTimeString_ r (drop 1 n)
  | null n = 0.0
  | otherwise = parseTimeString_ r (drop 1 n)

parseTimeString :: String -> Number
parseTimeString = parseTimeString_ ""

class HasUser a where
  setUser :: String -> a -> a
  getUser :: a -> String


data Group = Group
  { user :: String
  , group :: String
  , name :: String
  , repeat :: Number
  , created_at :: Number
  }

instance groupShow :: Show Group where
  show (Group g) = "Group<<" <> g.group <> ">><<" <> g.name <> ">>"

instance groupDecode :: Decode Group where
  decode o = do
    uid <- decode =<< readProp "user" o
    g <- decode =<< readProp "group" o
    n <- decode =<< readProp "name" o
    r <- decode =<< readProp "repeat" o
    ct <- decode =<< readProp "created_at" o
    pure $ Group
      { user: uid
      , group: g
      , name: n
      , repeat: r
      , created_at: ct
      }

instance groupHasUser :: HasUser Group where
  setUser uid (Group g) = Group (g {user = uid})
  getUser (Group g) = g.user

mkGroup :: String -> String -> Group
mkGroup g n = Group
  { user: "", group: g, name: n, repeat: 0.0, created_at: 0.0 }

data Message = Message
  { user :: String
  , group :: String
  , seq :: String
  , content :: String
  , sched_at :: Number
  , created_at :: Number
  }

instance messageShow :: Show Message where
  show (Message u) = "Message<<" <> u.group <> ">><<" <> u.seq <> ">>"

instance messageDecode :: Decode Message where
  decode o = do
    uid <- decode =<< readProp "user" o
    g <- decode =<< readProp "group" o
    s <- decode =<< readProp "seq" o
    c <- decode =<< readProp "content" o
    sc <- decode =<< readProp "sched_at" o
    ct <- decode =<< readProp "created_at" o
    pure $ Message
      { user: uid
      , group: g
      , seq: s
      , content: c
      , sched_at: sc
      , created_at: ct
      }

instance messageHasUser :: HasUser Message where
  setUser uid (Message m) = Message (m {user = uid})
  getUser (Message m) = m.user

message :: String -> String -> Message
message group seq = Message
  { user: ""
  , group: group
  , seq: seq
  , content: ""
  , sched_at: 0.0
  , created_at: 0.0
  }

setContent :: String -> Message -> Message
setContent content (Message m) = Message (m {content = content})

setSchedAt :: Number -> Message -> Message
setSchedAt schedat (Message m) = Message (m {sched_at = schedat})

exceptToMaybe :: forall a. F a -> Maybe a
exceptToMaybe a = case runExcept a of
                    Left _ -> Nothing
                    Right v -> Just v

decodeMaybe :: forall a. Decode a => Maybe Foreign -> Maybe a
decodeMaybe Nothing = Nothing
decodeMaybe (Just v) = exceptToMaybe $ decode v

toAff' :: forall a eff. Decode a => Promise (Maybe Foreign) -> Aff (db :: DB | eff) (Maybe a)
toAff' p = decodeMaybe <$> toAff p

foreign import _saveGroup :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getGroup :: forall a eff. String -> (a -> Maybe a) -> Maybe a -> Eff (db :: DB | eff) (Promise (Maybe a))
foreign import _setGroupRepeat :: forall eff. String -> Number -> Eff (db :: DB | eff) (Promise Unit)
foreign import _createMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _updateMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getMessage :: forall a b eff. a -> (b -> Maybe b) -> Maybe b -> Eff (db :: DB | eff) (Promise (Maybe b))
foreign import _getMessageList :: forall a eff. String -> Eff (db :: DB | eff) (Promise a)
foreign import _deleteMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _subscribeMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _unSubscribeMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getSubscribeList :: forall eff. String -> Eff (db :: DB | eff) (Promise (Array String))
foreign import _roomSubscribeMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _unRoomSubscribeMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getRoomSubscribeList :: forall eff. String -> Eff (db :: DB | eff) (Promise (Array String))

saveGroup :: forall eff. Group -> Aff (db :: DB | eff) Unit
saveGroup (Group u) = liftEff (_saveGroup u) >>= toAff

getGroup :: forall eff. String -> Aff (db :: DB | eff) (Maybe Group)
getGroup group = liftEff (_getGroup group Just Nothing) >>= toAff'

setGroupRepeat :: forall eff. String -> Number -> Aff (db :: DB | eff) Unit
setGroupRepeat g repeat = liftEff (_setGroupRepeat g repeat) >>= toAff

createMessage :: forall eff. Message -> Aff (db :: DB | eff) Unit
createMessage (Message m) = liftEff (_createMessage m) >>= toAff

updateMessage :: forall eff. Message -> Aff (db :: DB | eff) Unit
updateMessage (Message m) = liftEff (_updateMessage m) >>= toAff

getMessage :: forall eff. String -> String -> Aff (db :: DB | eff) (Maybe Message)
getMessage group seq = liftEff (_getMessage {group: group, seq: seq} Just Nothing) >>= toAff'

getMessageList :: forall eff. String -> Aff (db :: DB | eff) (Array Message)
getMessageList group = do
  ret <- liftEff (_getMessageList group) >>= toAff
  case runExcept (decode ret) of
    Left _ -> pure []
    Right v -> pure v

deleteMessage :: forall eff. String -> String -> Aff (db :: DB | eff) Unit
deleteMessage group seq =
  liftEff (_deleteMessage {group: group, seq: seq}) >>= toAff

subscribeMessage :: forall eff. String -> String -> Aff (db :: DB | eff) Unit
subscribeMessage user group =
  liftEff (_subscribeMessage {group: group, user: user}) >>= toAff

unSubscribeMessage :: forall eff. String -> String -> Aff (db :: DB | eff) Unit
unSubscribeMessage user group =
  liftEff (_unSubscribeMessage {group: group, user: user}) >>= toAff

getSubscribeList :: forall eff. String -> Aff (db :: DB | eff) (Array String)
getSubscribeList group = liftEff (_getSubscribeList group) >>= toAff

roomSubscribeMessage :: forall eff. String -> String -> Aff (db :: DB | eff) Unit
roomSubscribeMessage room group =
  liftEff (_roomSubscribeMessage {group: group, room: room}) >>= toAff

unRoomSubscribeMessage :: forall eff. String -> String -> Aff (db :: DB | eff) Unit
unRoomSubscribeMessage room group =
  liftEff (_unRoomSubscribeMessage {group: group, room: room}) >>= toAff

getRoomSubscribeList :: forall eff. String -> Aff (db :: DB | eff) (Array String)
getRoomSubscribeList group = liftEff (_getRoomSubscribeList group) >>= toAff
