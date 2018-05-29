module Chat
  (
    launchChat
  , Options (..)
  ) where

import Prelude

import Plan.Trans (PlanT, respond, param, ActionT, paramPattern, options, exit)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Data.String (trim, joinWith, drop)
import Wechaty.Contact (Contact) as C
import Wechaty.Contact (getContactName)
import Wechaty.Room (Room) as R
import Wechaty.Room (memberAll, runRoomT, delete)
import Data.Maybe (Maybe (..))
import Data.FormURLEncoded (fromArray, encode)
import Data.Tuple (Tuple (..))
import Utils (fetchJSON)
import Data.Argonaut.Core (toString, toObject, toArray, Json, caseJsonObject)
import Foreign.Object (lookup)
import Data.Array (catMaybes, concatMap)

data Options = Contact C.Contact
             | Room R.Room Boolean
             | Manager C.Contact

type ChatM = PlanT Options String Aff
type ActionM = ActionT Options Aff

searchHandler :: ActionM String
searchHandler = do
  keyword <- trim <$> param "keyword"
  ret <- liftAff
    $ fetchJSON ("http://111.230.171.235:6000/api/search/?"
    <> encode (fromArray [Tuple "q" (Just keyword)])) unit

  let arr = toObject ret >>= lookup "hits" >>= toArray
  case arr of
    Just arr' -> pure $ joinWith "\n"
                      $ catMaybes
                      $ concatMap go arr'
    Nothing -> pure "请尝试一下其它关键词"
  where go :: Json -> Array (Maybe String)
        go = caseJsonObject [] (\v -> [lookup "uri" v >>= toString, lookup "title" v >>= toString])

removeRoomMember :: ActionM String
removeRoomMember = do
  opts <- options
  name <- trim <$> param "name"
  case opts of
    Room r true -> runRoomT r $ do
      cs <- memberAll $ drop 1 name
      case cs of
        [] -> pure "没有找到用户"
        [c] -> do
          delete c
          pure "删除成功"
        xs -> pure $ "找到:\n" <> (joinWith "\n" $ map getContactName xs)
    _ -> exit

launchChat :: ChatM Unit
launchChat = do
  respond (paramPattern "search:keyword:") searchHandler
  respond (paramPattern "查找:keyword:") searchHandler
  respond (paramPattern "搜索:keyword:") searchHandler
  respond (paramPattern "删除:name:") removeRoomMember
  respond (paramPattern "rm:name:") removeRoomMember
  respond (paramPattern "del:name:") removeRoomMember
