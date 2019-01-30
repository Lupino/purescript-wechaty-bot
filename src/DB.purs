module DB
  ( Message (..)
  , messageMod
  ) where

import Prelude

import Config (dsn)
import Database.Sequelize (ModelOf, Sequelize, connect, define, string, timestamp, uint)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?), (.!=))

sequelize :: Sequelize
sequelize = unsafePerformEffect $ connect dsn {}

ct :: forall a. {type :: a, defaultValue :: Effect Int}
ct = {type: uint 10, defaultValue: timestamp}

options :: forall a. Array a -> { timestamps :: Boolean, charset :: String, collate :: String, indexes :: Array a }
options indexes =
  { timestamps: false
  , charset: "utf8"
  , collate: "utf8_unicode_ci"
  , indexes: indexes
  }

data Message = Message
  { id :: Int
  , user :: String
  , message :: String
  , sched_at :: Int
  , repeat :: String
  }

instance decodeJsonMessage :: DecodeJson Message where
  decodeJson j = do
    o <- decodeJson j
    id <- o .: "id"
    user <- o .: "user"
    message <- o .: "message"
    sched_at <- o .: "sched_at"
    repeat <- o .:? "repeat" .!= ""
    pure $ Message
      { id: id
      , user: user
      , message: message
      , sched_at: sched_at
      , repeat: repeat
      }

messageMod :: ModelOf Message
messageMod = define sequelize "message"
  { user: string 150
  , message: string 1500
  , sched_at: ct
  , repeat: {type: string 150, defaultValue: ""}
  }
  $ options []
