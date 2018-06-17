module Utils
  ( formatDate
  , startsWith
  , fetchJSON
  , adjustTime
  ) where

import Prelude hiding (add)

import Control.Promise (Promise, toAff)
import Data.Argonaut.Core (Json)
import Data.Dayjs (Dayjs, format, fromUnixTime, add)
import Data.String (drop, take, null)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

foreign import startsWith :: String -> String -> Boolean
foreign import _fetchJSON :: forall a. String -> a -> Effect (Promise Json)

fetchJSON :: forall a. String -> a -> Aff Json
fetchJSON url opts = liftEffect (_fetchJSON url opts) >>= toAff

-- 1d 10h 10m 10s
adjustTime_ :: String -> String -> Dayjs -> Dayjs
adjustTime_ r n
  | startsWith n "Y" = add r "year" <<< adjustTime_ "" (drop 1 n)
  | startsWith n "M" = add r "month" <<< adjustTime_ "" (drop 1 n)
  | startsWith n "d" = add r "day" <<< adjustTime_ "" (drop 1 n)
  | startsWith n "h" = add r "hour" <<< adjustTime_ "" (drop 1 n)
  | startsWith n "m" = add r "minute" <<< adjustTime_ "" (drop 1 n)
  | startsWith n "s" = add r "second" <<< adjustTime_ "" (drop 1 n)
  | startsWith n " " = adjustTime_ r (drop 1 n)
  | null n = \id -> id
  | otherwise = adjustTime_ (r <> take 1 n) (drop 1 n)

adjustTime :: String -> Dayjs -> Dayjs
adjustTime = adjustTime_ ""

formatDate :: Int -> String
formatDate = format "YYYY-MM-DD HH:mm:ss" <<< fromUnixTime
