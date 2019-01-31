module Chat
  ( launchChat
  , Options (..)
  ) where

import Prelude

import Config (searchHost)
import Data.Argonaut.Core (toString, toObject, toArray, Json, caseJsonObject)
import Data.Array (catMaybes, concatMap, take)
import Data.FormURLEncoded (fromArray, encode)
import Data.Maybe (Maybe(..))
import Data.String (trim, joinWith, drop)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Foreign.Object (lookup)
import Plan.Trans (ActionT, PlanT, exit, options, param, paramPattern, respond)
import Utils (fetchJSON)
import Wechaty.Contact (Contact, name') as C
import Wechaty.Room (Room) as R
import Wechaty.Room (memberAll, runRoomT, del)

data Options = Contact C.Contact
             | Room R.Room Boolean
             | Manager C.Contact

type ChatM = PlanT Options String Aff
type ActionM = ActionT Options Aff

searchHandler :: ActionM String
searchHandler = do
  keyword <- trim <$> param "keyword"
  ret <- liftAff
    $ fetchJSON
      ( "http://"
      <> searchHost
      <> "/api/search/?"
      <> encode (fromArray [Tuple "q" (Just keyword)])) unit

  case toObject ret >>= lookup "hits" >>= toArray of
    Just arr -> pure $ joinWith "\n"
                     $ catMaybes
                     $ concatMap go
                     $ take 3 arr
    Nothing -> pure "请尝试一下其它关键词"
  where go :: Json -> Array (Maybe String)
        go = caseJsonObject [] (\v -> [lookup "uri" v >>= toString, lookup "title" v >>= toString])

removeRoomMemberHandler :: ActionM String
removeRoomMemberHandler = do
  opts <- options
  name <- trim <$> param "name"
  case opts of
    Room r true -> runRoomT r $ do
      cs <- memberAll $ drop 1 name
      case cs of
        [] -> pure "没有找到用户"
        [c] -> do
          del c
          pure "删除成功"
        xs -> pure $ "找到:\n" <> (joinWith "\n" $ map C.name' xs)
    _ -> exit

helloHandler :: String -> String -> ActionM String
helloHandler prefix subfix = do
  word <- trim <$> param "word"
  pure (prefix <> word <> subfix)


launchChat :: ChatM Unit
launchChat = do
  respond (paramPattern "search:keyword:") searchHandler
  respond (paramPattern "查找:keyword:") searchHandler
  respond (paramPattern "搜索:keyword:") searchHandler
  respond (paramPattern "删除:name:") removeRoomMemberHandler
  respond (paramPattern "rm:name:") removeRoomMemberHandler
  respond (paramPattern "del:name:") removeRoomMemberHandler
  respond (paramPattern "你好:word:") $ helloHandler "你好" "!"
  respond (paramPattern "新年快乐:word:") $ helloHandler "新年快乐" "!"
