module Utils
  ( parseTimeString
  , formatTimeString
  , startsWith
  , convertSchedAt
  , momentFormat
  , getTimeStamp
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Data.String (null, take, drop)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Effect.Now (now)
import Math (floor)
import Control.Promise (Promise, toAff)
import Effect.Aff (Aff)
import Data.Argonaut.Core (Json)

foreign import startsWith :: String -> String -> Boolean
foreign import convertSchedAt :: forall a. a -> Number
foreign import momentFormat :: Number -> String -> String
foreign import formatTimeString :: Number -> String
foreign import readNumber :: String -> Number

foreign import _fetchJSON :: forall a. String -> a -> Effect (Promise Json)

-- 1d 10h 10m 10s
parseTimeString_ :: String -> String -> Number
parseTimeString_ r n
  | startsWith n "d" = readNumber r * 24.0 * 60.0 * 60.0 + parseTimeString_ "" (drop 1 n)
  | startsWith n "h" = readNumber r * 60.0 * 60.0 + parseTimeString_ "" (drop 1 n)
  | startsWith n "m" = readNumber r * 60.0 + parseTimeString_ "" (drop 1 n)
  | startsWith n "s" = readNumber r + parseTimeString_ "" (drop 1 n)
  | startsWith n " " = parseTimeString_ r (drop 1 n)
  | null n = 0.0
  | otherwise = parseTimeString_ (r <> take 1 n) (drop 1 n)

parseTimeString :: String -> Number
parseTimeString = parseTimeString_ ""

getTimeStamp :: Effect Number
getTimeStamp = do
  (Milliseconds n) <- map unInstant now
  pure $ floor (n / 1000.0)

fetchJSON :: forall a. String -> a -> Aff Json
fetchJSON url opts = liftEffect (_fetchJSON url opts) >>= toAff
