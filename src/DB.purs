module DB
  ( Message
  , messageMod
  ) where

import Prelude

import Config (dsn)
import Database.Sequelize (ModelOf, Sequelize, connect, define, string, timestamp, uint)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)

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

foreign import data Message :: Type

messageMod :: ModelOf Message
messageMod = define sequelize "message"
  { user: string 150
  , message: string 1500
  , sched_at: ct
  , repeat: {type: string 150, defaultValue: ""}
  }
  $ options []
