module Chat
  (
    launchChat
  , Options (..)
  ) where

import Prelude

import Config (searchHost)
import Data.Argonaut.Core (toString, toObject, toArray, Json, caseJsonObject)
import Data.Array (catMaybes, concatMap, take)
import Data.Either (Either(..))
import Data.FormURLEncoded (fromArray, encode)
import Data.Maybe (Maybe(..))
import Data.String (trim, joinWith, drop)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Exception (message)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (lookup)
import Mal (rep, stepEnv)
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
          del c
          pure "删除成功"
        xs -> pure $ "找到:\n" <> (joinWith "\n" $ map C.name' xs)
    _ -> exit

malHandler :: ActionM String
malHandler = do
  str <- trim <$> param "str"
  env <- liftEffect stepEnv
  r <- liftEffect $ rep env str
  case r of
    Left e -> pure $ message e
    Right s -> pure s


launchChat :: ChatM Unit
launchChat = do
  respond (paramPattern "search:keyword:") searchHandler
  respond (paramPattern "查找:keyword:") searchHandler
  respond (paramPattern "搜索:keyword:") searchHandler
  respond (paramPattern "删除:name:") removeRoomMember
  respond (paramPattern "rm:name:") removeRoomMember
  respond (paramPattern "del:name:") removeRoomMember
  respond (paramPattern ">:str:") malHandler
  respond (paramPattern "&gt;:str:") malHandler
