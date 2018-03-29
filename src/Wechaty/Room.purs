module Wechaty.Room
    ( find
    , say
    , sayTo
    , getRoomId
    , getRoomTopic
    , roomId
    , roomTopic
    ) where

import Prelude
import Data.Maybe (Maybe (..))
import Control.Promise (Promise, toAff)
import Wechaty.Types (Room, RoomM, Contact)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Function.Uncurried (Fn2, Fn3, runFn2, runFn3)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)

foreign import _find :: forall eff. String
                     -> (Room -> Maybe Room)
                     -> Maybe Room
                     -> Eff eff (Promise (Maybe Room))

find :: forall eff. String -> Aff eff (Maybe Room)
find n = liftEff (_find n Just Nothing) >>= toAff

foreign import _say :: forall a eff. Fn2 Room a (Eff eff (Promise Unit))

runSay :: forall a eff. Room -> a -> Aff eff Unit
runSay room a = liftEff (runFn2 _say room a) >>= toAff

say :: forall a eff. a -> RoomM eff Unit
say a = do
  room <- ask
  lift $ runSay room a

foreign import _sayTo :: forall a eff. Fn3 Room Contact a (Eff eff (Promise Unit))

runSayTo :: forall a eff. Room -> Contact -> a -> Aff eff Unit
runSayTo room contact a = liftEff (runFn3 _sayTo room contact a) >>= toAff

sayTo :: forall a eff. Contact -> a -> RoomM eff Unit
sayTo contact a = do
  room <- ask
  lift $ runSayTo room contact a

roomId :: forall eff. RoomM eff String
roomId = getRoomId <$> ask

roomTopic :: forall eff. RoomM eff String
roomTopic = getRoomTopic <$> ask

foreign import getRoomId :: Room -> String
foreign import getRoomTopic :: Room -> String
