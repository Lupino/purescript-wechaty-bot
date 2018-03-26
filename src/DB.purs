module DB
  ( DB
  , User (..)
  , user
  , saveUser
  , getUser

  , Message (..)
  , message
  , setContent
  , setUserId
  , setSchedAt

  , createMessage
  , getMessage
  , getMessageList
  , deleteMessage

  , subscribeMessage
  , unSubscribeMessage
  , getSubscribeList
  ) where

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Control.Promise (Promise, toAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Foreign (F, Foreign)
import Control.Monad.Except (runExcept)
import Control.Monad.Eff (kind Effect)

foreign import data DB :: Effect

data User = User
  { userid :: String
  , name :: String
  , created_at :: Int
  }

user :: String -> String -> User
user uid n = User {userid: uid, name: n, created_at: 0}

instance userShow :: Show User where
  show (User u) = "User<<" <> u.userid <> ">><<" <> u.name <> ">>"

instance userDecode :: Decode User where
  decode o = do
     uid <- decode =<< readProp "userid" o
     n <- decode =<< readProp "name" o
     ct <- decode =<< readProp "created_at" o
     pure $ User {userid: uid, name: n, created_at: ct}

data Message = Message
  { userid :: String
  , group :: String
  , seq :: String
  , content :: String
  , sched_at :: Int
  , created_at :: Int
  }

instance messageShow :: Show Message where
  show (Message u) = "Message<<" <> u.group <> ">><<" <> u.seq <> ">>"

instance messageDecode :: Decode Message where
  decode o = do
    uid <- decode =<< readProp "userid" o
    g <- decode =<< readProp "group" o
    s <- decode =<< readProp "seq" o
    c <- decode =<< readProp "content" o
    sc <- decode =<< readProp "sched_at" o
    ct <- decode =<< readProp "created_at" o
    pure $ Message
      { userid: uid
      , group: g
      , seq: s
      , content: c
      , sched_at: sc
      , created_at: ct
      }

message :: String -> String -> Message
message group seq = Message
  { userid: ""
  , group: group
  , seq: seq
  , content: ""
  , sched_at: 0
  , created_at: 0
  }

setContent :: String -> Message -> Message
setContent content (Message m) = Message (m {content = content})

setUserId :: String -> Message -> Message
setUserId userid (Message m) = Message (m {userid = userid})

setSchedAt :: Int -> Message -> Message
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

foreign import _saveUser :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getUser :: forall a eff. String -> (a -> Maybe a) -> Maybe a -> Eff (db :: DB | eff) (Promise (Maybe a))
foreign import _createMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getMessage :: forall a b eff. a -> (b -> Maybe b) -> Maybe b -> Eff (db :: DB | eff) (Promise (Maybe b))
foreign import _getMessageList :: forall a eff. String -> Eff (db :: DB | eff) (Promise a)
foreign import _deleteMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _subscribeMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _unSubscribeMessage :: forall a eff. a -> Eff (db :: DB | eff) (Promise Unit)
foreign import _getSubscribeList :: forall eff. String -> Eff (db :: DB | eff) (Promise (Array String))

saveUser :: forall eff. User -> Aff (db :: DB | eff) Unit
saveUser (User u) = liftEff (_saveUser u) >>= toAff

getUser :: forall eff. String -> Aff (db :: DB | eff) (Maybe User)
getUser userid = liftEff (_getUser userid Just Nothing) >>= toAff'

createMessage :: forall eff. Message -> Aff (db :: DB | eff) Unit
createMessage (Message m) = liftEff (_createMessage m) >>= toAff

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
subscribeMessage userid group =
  liftEff (_subscribeMessage {group: group, userid: userid}) >>= toAff

unSubscribeMessage :: forall eff. String -> String -> Aff (db :: DB | eff) Unit
unSubscribeMessage userid group =
  liftEff (_unSubscribeMessage {group: group, userid: userid}) >>= toAff

getSubscribeList :: forall eff. String -> Aff (db :: DB | eff) (Array String)
getSubscribeList group = liftEff (_getSubscribeList group) >>= toAff