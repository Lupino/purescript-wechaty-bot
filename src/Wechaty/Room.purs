module Wechaty.Room
  ( Room
  , RoomT
  , runRoomT
  , find
  , say
  , sayTo
  , getRoomTopic
  , roomTopic
  , module Wechaty.Types
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Data.Maybe (Maybe(..))
import Wechaty.Contact (Contact)
import Wechaty.Types (WECHATY)

foreign import data Room :: Type
type RoomT m = ReaderT Room m

runRoomT :: forall a m. Room -> RoomT m a -> m a
runRoomT room = flip runReaderT room

foreign import _find :: forall eff. String
                     -> (Room -> Maybe Room)
                     -> Maybe Room
                     -> Eff eff (Promise (Maybe Room))

find :: forall eff. String -> Aff eff (Maybe Room)
find n = liftEff (_find n Just Nothing) >>= toAff

foreign import _say :: forall a eff. Fn2 Room a (Eff eff (Promise Unit))

runSay :: forall a eff. Room -> a -> Aff eff Unit
runSay room a = liftEff (runFn2 _say room a) >>= toAff

say
  :: forall a m eff. MonadAff (wechaty :: WECHATY | eff) m
  => a -> RoomT m Unit
say a = do
  room <- ask
  liftAff $ runSay room a

foreign import _sayTo :: forall a eff. Fn3 Room Contact a (Eff eff (Promise Unit))

runSayTo :: forall a eff. Room -> Contact -> a -> Aff eff Unit
runSayTo room contact a = liftEff (runFn3 _sayTo room contact a) >>= toAff

sayTo
  :: forall a m eff. MonadAff (wechaty :: WECHATY | eff) m
  => Contact -> a -> RoomT m Unit
sayTo contact a = do
  room <- ask
  liftAff $ runSayTo room contact a

roomTopic
  :: forall m. Monad m
  => RoomT m String
roomTopic = getRoomTopic <$> ask

foreign import getRoomTopic :: Room -> String
