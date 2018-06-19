module Mal
  ( Env
  , stepEnv
  , rep
  ) where

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)

foreign import data Env :: Type

foreign import stepEnv :: Effect Env

foreign import _rep :: Env -> String -> (Error -> Either Error String) -> (String -> Either Error String) -> Effect (Either Error String)

rep :: Env -> String -> Effect (Either Error String)
rep env str = _rep env str Left Right
