module Chatter
  (
    launchChatter
  ) where

import Prelude

import Plan.Trans (PlanT, respond, param, Pattern (..), ActionT, Param (..))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (REF)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Array (mapWithIndex, catMaybes)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)
import Data.Either (fromRight)

type ChatterM eff = PlanT String (Aff (ref :: REF | eff))
type ActionM eff = ActionT (Aff (ref :: REF | eff))

regexPattern :: Regex -> Pattern
regexPattern reg = Pattern go
  where go :: String -> Maybe (Array Param)
        go xs = do
          m <- match reg xs
          pure $ mapWithIndex toParam $ catMaybes m
          where toParam :: Int -> String -> Param
                toParam idx v = Param (show idx) v


reSayHello :: Regex
reSayHello = unsafePartial $ fromRight $ regex "^你好$" noFlags

helloHandler :: forall eff. ActionM eff String
helloHandler = pure "谢谢, 我很好"

reSayHello1 :: Regex
reSayHello1 = unsafePartial $ fromRight $ regex "^你好\\s*我是(.+)$" noFlags

hello1Handler :: forall eff. ActionM eff String
hello1Handler = do
  name <- param "1"
  pure $ "Hi, " <> name <> "你好"

launchChatter :: forall eff. ChatterM eff Unit
launchChatter = do
  respond (regexPattern reSayHello) helloHandler
  respond (regexPattern reSayHello1) hello1Handler
