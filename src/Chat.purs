module Chat
  (
    launchChat
  , Options (..)
  ) where

import Prelude

import Plan.Trans (PlanT, respond, param, ActionT, paramPattern)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Data.String (trim, joinWith)
import Wechaty.Contact (Contact) as C
import Wechaty.Room (Room) as R
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

launchChat :: ChatM Unit
launchChat = do
  respond (paramPattern "search:keyword:") searchHandler
  respond (paramPattern "查找:keyword:") searchHandler
  respond (paramPattern "搜索:keyword:") searchHandler
