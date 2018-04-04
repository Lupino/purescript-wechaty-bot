module Utils
  ( parseTimeString
  , formatTimeString
  , startsWith
  , convertSchedAt
  , momentFormat
  , getTimeStamp
  ) where

import Prelude

import Control.Monad.Eff (Eff)
import Data.String (null, take, drop)
import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Control.Monad.Eff.Now (now, NOW)
import Math (floor)

foreign import startsWith :: String -> String -> Boolean
foreign import convertSchedAt :: forall a. a -> Number
foreign import momentFormat :: Number -> String -> String
foreign import formatTimeString :: Number -> String
foreign import readNumber :: String -> Number

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

getTimeStamp :: forall eff. Eff (now :: NOW | eff) Number
getTimeStamp = do
  (Milliseconds n) <- map unInstant now
  pure $ floor (n / 1000.0)
