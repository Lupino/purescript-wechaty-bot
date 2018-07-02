module Database.Sequelize
  ( Sequelize
  , ModelOf
  , connect
  , connectOpts
  , query_
  , query
  , define

  , string
  , text
  , json
  , int
  , uint
  , bigint
  , timestamp

  , findOne_
  , findOne
  , findAndCountAll_
  , findAndCountAll
  , findOrCreate_
  , findOrCreate
  , create_
  , create
  , update
  , destory
  , count
  , sum
  , decrement
  , increment
  , findAll_
  , findAll
  , upsert

  , sync
  , getTableName
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array (catMaybes)
import Data.Either (fromRight, hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)


foreign import data Sequelize :: Type

data ModelOf a = ModelOf a

decodeJsonArray :: forall a. DecodeJson a => Array Json -> Array a
decodeJsonArray = catMaybes <<< map (hush <<< decodeJson)

unsafeDecodeJson :: forall a. DecodeJson a => Json -> a
unsafeDecodeJson = unsafePartial $ fromRight <<< decodeJson

foreign import connect :: forall opts. String -> opts -> Effect Sequelize

foreign import connectOpts :: forall opts. opts -> Effect Sequelize

foreign import _query :: forall opts a. Sequelize -> String -> opts -> Effect (Promise (Array a))

query_ :: forall opts. Sequelize -> String -> opts -> Aff (Array Json)
query_ seq sql opts = liftEffect (_query seq sql opts) >>= toAff

query :: forall opts a. DecodeJson a => Sequelize -> String -> opts -> Aff (Array a)
query seq sql opts = decodeJsonArray <$> query_ seq sql opts

foreign import define :: forall struct opts a. Sequelize -> String -> struct -> opts -> ModelOf a

foreign import string :: forall a. Int -> a
foreign import text :: forall a. a
foreign import json :: forall a. a
foreign import uint :: forall a. Int -> a
foreign import int :: forall a. Int -> a
foreign import bigint :: forall a. Int -> a
foreign import timestamp :: Effect Int

foreign import _findOne
  :: forall a opts. ModelOf a -> (Json -> Maybe Json) -> Maybe Json -> opts -> Effect (Promise (Maybe Json))

findOne_ :: forall a opts. ModelOf a -> opts -> Aff (Maybe Json)
findOne_ mod opts = liftEffect (_findOne mod Just Nothing opts) >>= toAff

findOne :: forall a opts. DecodeJson a => ModelOf a -> opts -> Aff (Maybe a)
findOne mod opts = go <$> findOne_ mod opts
  where go :: forall a0. DecodeJson a0 => Maybe Json -> Maybe a0
        go Nothing = Nothing
        go (Just v) = hush $ decodeJson v

foreign import _update
  :: forall a updated opts. ModelOf a -> updated -> opts -> Effect (Promise Unit)

update :: forall a updated opts. ModelOf a -> updated -> opts -> Aff Unit
update mod updated opts = liftEffect (_update mod updated opts) >>= toAff

foreign import _create :: forall a obj. ModelOf a -> obj -> Effect (Promise Json)

create_ :: forall a obj. ModelOf a -> obj -> Aff Json
create_ mod obj = liftEffect (_create mod obj) >>= toAff

create :: forall a obj. DecodeJson a => ModelOf a -> obj -> Aff a
create mod obj = unsafeDecodeJson <$> create_ mod obj

foreign import _sync :: forall a opts. ModelOf a -> opts -> Effect (Promise Unit)

sync :: forall a opts. ModelOf a -> opts -> Aff Unit
sync mod opts = liftEffect (_sync mod opts) >>= toAff

foreign import _destory
  :: forall a opts. ModelOf a -> opts -> Effect (Promise Unit)

destory :: forall a opts. ModelOf a -> opts -> Aff Unit
destory mod opts = liftEffect (_destory mod opts) >>= toAff

foreign import _findAndCountAll
  :: forall a opts
  . ModelOf a -> opts
  -> (Int -> Array Json -> Tuple Int (Array Json))
  -> Effect (Promise (Tuple Int (Array Json)))

findAndCountAll_ :: forall a opts. ModelOf a -> opts -> Aff (Tuple Int (Array Json))
findAndCountAll_ mod opts = liftEffect (_findAndCountAll mod opts Tuple) >>= toAff

findAndCountAll :: forall a opts. DecodeJson a => ModelOf a -> opts -> Aff (Tuple Int (Array a))
findAndCountAll mod opts = map decodeJsonArray <$> findAndCountAll_ mod opts

foreign import _findOrCreate
    :: forall a opts. ModelOf a -> opts -> Effect (Promise Json)

findOrCreate_ :: forall a opts. ModelOf a -> opts -> Aff Json
findOrCreate_ mod opts = liftEffect (_findOrCreate mod opts) >>= toAff

findOrCreate :: forall a opts. DecodeJson a => ModelOf a -> opts -> Aff a
findOrCreate mod opts = unsafeDecodeJson <$> findOrCreate_ mod opts

foreign import _count :: forall a opts. ModelOf a -> opts -> Effect (Promise Int)

count :: forall a opts. ModelOf a -> opts -> Aff Int
count mod opts = liftEffect (_count mod opts) >>= toAff

foreign import _decrement :: forall a fields opts. ModelOf a -> fields -> opts -> Effect (Promise Unit)

decrement :: forall a fields opts. ModelOf a -> fields -> opts -> Aff Unit
decrement mod fields opts = liftEffect (_decrement mod fields opts) >>= toAff

foreign import _increment :: forall a fields opts. ModelOf a -> fields -> opts -> Effect (Promise Unit)

increment :: forall a fields opts. ModelOf a -> fields -> opts -> Aff Unit
increment mod fields opts = liftEffect (_increment mod fields opts) >>= toAff

foreign import _findAll :: forall a opts. ModelOf a -> opts -> Effect (Promise (Array Json))

findAll_ :: forall a opts. ModelOf a -> opts -> Aff (Array Json)
findAll_ mod opts = liftEffect (_findAll mod opts) >>= toAff

findAll :: forall a opts. DecodeJson a => ModelOf a -> opts -> Aff (Array a)
findAll mod opts = decodeJsonArray <$> findAll_ mod opts

foreign import _upsert :: forall a values opts. ModelOf a -> values -> opts -> Effect (Promise Boolean)

upsert :: forall a values opts. ModelOf a -> values -> opts -> Aff Boolean
upsert mod values opts = liftEffect (_upsert mod values opts) >>= toAff

foreign import _sum :: forall a opts. ModelOf a -> String -> opts -> Effect (Promise Number)

sum :: forall a opts. ModelOf a -> String -> opts -> Aff Number
sum mod field opts = liftEffect (_sum mod field opts) >>= toAff

foreign import getTableName :: forall a. ModelOf a -> String
