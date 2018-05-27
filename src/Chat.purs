module Chat
  (
    launchChat
  , Options (..)
  ) where

import Prelude

import Plan.Trans (PlanT, respond, param, ActionT, regexPattern, paramPattern)
import Effect.Aff (Aff)
import Data.String (trim)
import Wechaty.Contact (Contact) as C
import Wechaty.Room (Room) as R
import Data.Maybe (Maybe (..))

data Options = Contact C.Contact
             | Room R.Room Boolean
             | Manager C.Contact

type ChatM = PlanT Options String Aff
type ActionM = ActionT Options Aff

helloHandler :: ActionM String
helloHandler = pure "谢谢, 我很好"

hello1Handler :: ActionM String
hello1Handler = do
  name <- param "1"
  pure $ "Hi, " <> name <> "你好"

hello2Handler :: ActionM String
hello2Handler = do
  name <- trim <$> param "name"
  pure $ "Hi, " <> name <> "你好"

hello3Handler :: ActionM String
hello3Handler = do
  name <- trim <$> param "name"
  name1 <- trim <$> param "name1"
  pure $ "Hi, " <> name <> "你好, " <> name1 <> "你好"

searchHandler :: ActionM String
searchHandler = do
  keyword <- trim <$> param "keyword"
  pure keyword

launchChat :: ChatM Unit
launchChat = do
  respond (regexPattern "^你好$") helloHandler
  respond (regexPattern "^你好\\s*我是(.+)$") hello1Handler
  respond (paramPattern "你好:name:,:name1:") hello3Handler
  respond (paramPattern "你好:name:") hello2Handler
