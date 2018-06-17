module Utils
  ( formatDate
  , startsWith
  , fetchJSON
  , parseTimeString
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Argonaut.Core (Json)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Data.Int (fromString)
import Data.String (drop, take, null)
import Data.Maybe (fromMaybe)

foreign import formatDate :: Int -> String
foreign import startsWith :: String -> String -> Boolean
foreign import _fetchJSON :: forall a. String -> a -> Effect (Promise Json)

fetchJSON :: forall a. String -> a -> Aff Json
fetchJSON url opts = liftEffect (_fetchJSON url opts) >>= toAff

-- 1d 10h 10m 10s
parseTimeString_ :: String -> String -> Int
parseTimeString_ r n
  | startsWith n "d" = fromMaybe 0 (fromString r) * 24 * 60 * 60 + parseTimeString_ "" (drop 1 n)
  | startsWith n "h" = fromMaybe 0 (fromString r) * 60 * 60 + parseTimeString_ "" (drop 1 n)
  | startsWith n "m" = fromMaybe 0 (fromString r) * 60 + parseTimeString_ "" (drop 1 n)
  | startsWith n "s" = fromMaybe 0 (fromString r) + parseTimeString_ "" (drop 1 n)
  | startsWith n " " = parseTimeString_ r (drop 1 n)
  | null n = 0
  | otherwise = parseTimeString_ (r <> take 1 n) (drop 1 n)

parseTimeString :: String -> Int
parseTimeString = parseTimeString_ ""
