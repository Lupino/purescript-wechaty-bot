module Utils
  ( startsWith
  , fetchJSON
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise, toAff)
import Effect.Aff (Aff)
import Data.Argonaut.Core (Json)

foreign import startsWith :: String -> String -> Boolean
foreign import _fetchJSON :: forall a. String -> a -> Effect (Promise Json)

fetchJSON :: forall a. String -> a -> Aff Json
fetchJSON url opts = liftEffect (_fetchJSON url opts) >>= toAff
