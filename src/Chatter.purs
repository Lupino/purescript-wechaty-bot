module Chatter
  (
    launchChatter
  ) where

import Prelude

import Plan.Trans (PlanT, respond, param, ActionT, regexPattern, paramPattern)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Ref (REF)
import Data.String (trim)

type ChatterM eff = PlanT Unit String (Aff (ref :: REF | eff))
type ActionM eff = ActionT Unit (Aff (ref :: REF | eff))

helloHandler :: forall eff. ActionM eff String
helloHandler = pure "谢谢, 我很好"

hello1Handler :: forall eff. ActionM eff String
hello1Handler = do
  name <- param "1"
  pure $ "Hi, " <> name <> "你好"

hello2Handler :: forall eff. ActionM eff String
hello2Handler = do
  name <- trim <$> param "name"
  pure $ "Hi, " <> name <> "你好"

hello3Handler :: forall eff. ActionM eff String
hello3Handler = do
  name <- trim <$> param "name"
  name1 <- trim <$> param "name1"
  pure $ "Hi, " <> name <> "你好, " <> name1 <> "你好"

launchChatter :: forall eff. ChatterM eff Unit
launchChatter = do
  respond (regexPattern "^你好$") helloHandler
  respond (regexPattern "^你好\\s*我是(.+)$") hello1Handler
  respond (paramPattern "你好:name:,:name1:") hello3Handler
  respond (paramPattern "你好:name:") hello2Handler
