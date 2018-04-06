module Chatter
  (
    launchChatter
  ) where

import Prelude

import Plan.Trans (PlanT, respond, param, ActionT, regexPattern)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (REF)

type ChatterM eff = PlanT String (Aff (ref :: REF | eff))
type ActionM eff = ActionT (Aff (ref :: REF | eff))

helloHandler :: forall eff. ActionM eff String
helloHandler = pure "谢谢, 我很好"

hello1Handler :: forall eff. ActionM eff String
hello1Handler = do
  name <- param "1"
  pure $ "Hi, " <> name <> "你好"

launchChatter :: forall eff. ChatterM eff Unit
launchChatter = do
  respond (regexPattern "^你好$") helloHandler
  respond (regexPattern "^你好\\s*我是(.+)$") hello1Handler
