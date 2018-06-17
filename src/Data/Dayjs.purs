module Data.Dayjs where

import Effect (Effect)

foreign import data Dayjs :: Type

foreign import fromUnixTime :: Int -> Dayjs

foreign import toUnixTime :: Dayjs -> Int

foreign import set :: String -> Int -> Dayjs -> Dayjs

foreign import add :: forall a. a -> String -> Dayjs -> Dayjs

foreign import format :: String -> Dayjs -> String

foreign import now :: Effect Dayjs
