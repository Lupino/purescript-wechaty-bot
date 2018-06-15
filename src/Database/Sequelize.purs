module Database.Sequelize
  ( Sequelize
  , ModelOf (..)
  , connect
  , connect'
  , query
  , define

  , string
  , text
  , json
  , int
  , uint
  , timestamp

  , findOne
  , findAndCountAll
  , findOrCreate
  , create
  , update
  , destory
  , count
  , decrement
  , increment
  , findAll
  , upsert

  , sync
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)


foreign import data Sequelize :: Type

data ModelOf a = ModelOf a

foreign import connect :: forall opts. String -> opts -> Effect Sequelize

foreign import connect' :: forall opts. opts -> Effect Sequelize

foreign import _query :: forall opts a. Sequelize -> String -> opts -> Effect (Promise a)

query :: forall opts a. Sequelize -> String -> opts -> Aff a
query seq sql opts = liftEffect (_query seq sql opts) >>= toAff

foreign import define :: forall struct opts a. Sequelize -> String -> struct -> opts -> ModelOf a

foreign import string :: forall a. Int -> a
foreign import text :: forall a. a
foreign import json :: forall a. a
foreign import uint :: forall a. Int -> a
foreign import int :: forall a. Int -> a
foreign import timestamp :: Effect Int

foreign import _findOne
  :: forall a b opts. ModelOf a -> (b -> Maybe b) -> Maybe b -> opts -> Effect (Promise (Maybe b))

findOne :: forall a b opts. ModelOf a -> opts -> Aff (Maybe b)
findOne mod opts = liftEffect (_findOne mod Just Nothing opts) >>= toAff

foreign import _update
  :: forall a updated opts. ModelOf a -> updated -> opts -> Effect (Promise Unit)

update :: forall a updated opts. ModelOf a -> updated -> opts -> Aff Unit
update mod updated opts = liftEffect (_update mod updated opts) >>= toAff

foreign import _create :: forall a b obj. ModelOf a -> obj -> Effect (Promise b)

create :: forall a b obj. ModelOf a -> obj -> Aff b
create mod obj = liftEffect (_create mod obj) >>= toAff

foreign import _sync :: forall a opts. ModelOf a -> opts -> Effect (Promise Unit)

sync :: forall a opts. ModelOf a -> opts -> Aff Unit
sync mod opts = liftEffect (_sync mod opts) >>= toAff

foreign import _destory
  :: forall a opts. ModelOf a -> opts -> Effect (Promise Unit)

destory :: forall a opts. ModelOf a -> opts -> Aff Unit
destory mod opts = liftEffect (_destory mod opts) >>= toAff

foreign import _findAndCountAll
  :: forall a b opts
  . ModelOf a -> opts
  -> (Int -> Array b -> Tuple Int (Array b))
  -> Effect (Promise (Tuple Int (Array b)))

findAndCountAll :: forall a b opts. ModelOf a -> opts -> Aff (Tuple Int (Array b))
findAndCountAll mod opts = liftEffect (_findAndCountAll mod opts Tuple) >>= toAff

foreign import _findOrCreate
    :: forall a b opts. ModelOf a -> opts -> Effect (Promise b)

findOrCreate :: forall a b opts. ModelOf a -> opts -> Aff b
findOrCreate mod opts = liftEffect (_findOrCreate mod opts) >>= toAff

foreign import _count :: forall a opts. ModelOf a -> opts -> Effect (Promise Int)

count :: forall a opts. ModelOf a -> opts -> Aff Int
count mod opts = liftEffect (_count mod opts) >>= toAff

foreign import _decrement :: forall a fields opts. ModelOf a -> fields -> opts -> Effect (Promise Unit)

decrement :: forall a fields opts. ModelOf a -> fields -> opts -> Aff Unit
decrement mod fields opts = liftEffect (_decrement mod fields opts) >>= toAff

foreign import _increment :: forall a fields opts. ModelOf a -> fields -> opts -> Effect (Promise Unit)

increment :: forall a fields opts. ModelOf a -> fields -> opts -> Aff Unit
increment mod fields opts = liftEffect (_increment mod fields opts) >>= toAff

foreign import _findAll :: forall a b opts. ModelOf a -> opts -> Effect (Promise (Array b))

findAll :: forall a b opts. ModelOf a -> opts -> Aff (Array b)
findAll mod opts = liftEffect (_findAll mod opts) >>= toAff

foreign import _upsert :: forall a values opts. ModelOf a -> values -> opts -> Effect (Promise Boolean)

upsert :: forall a values opts. ModelOf a -> values -> opts -> Aff Boolean
upsert mod values opts = liftEffect (_upsert mod values opts) >>= toAff

foreign import _sum :: forall a opts. ModelOf a -> String -> opts -> Effect (Promise Number)

sum :: forall a opts. ModelOf a -> String -> opts -> Aff Number
sum mod field opts = liftEffect (_sum mod field opts) >>= toAff
