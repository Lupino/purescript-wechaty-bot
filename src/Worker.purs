module Worker
  ( launchWorker
  ) where

import Prelude

import Config (get)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (now, NOW)
import Control.Monad.Error.Class (try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import DB (DB, Group(..), Message(..), getGroup, getMessage, getRoomSubscribeList, getSubscribeList, setSchedAt, updateMessage)
import Data.Array ((!!), head, tail, null)
import Data.DateTime.Instant (unInstant)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), split)
import Data.Time.Duration (Milliseconds(..))
import Math as M
import Partial.Unsafe (unsafePartial)
import Periodic.Worker (PERIODIC, addFunc, done, name, runWorkerT, schedLater, work)
import Wechaty.Contact (say, find)
import Wechaty.Room as R
import Wechaty.Types (WECHATY, runContactM, runRoomM)

type TaskM eff = MaybeT (Aff (wechaty :: WECHATY, db :: DB, now :: NOW | eff))

launchWorker :: forall eff. Aff (periodic :: PERIODIC, db :: DB, wechaty :: WECHATY, now :: NOW | eff) Unit
launchWorker = do
  runWorkerT launchAff_ (get "periodic") $ do
    addFunc "send-message" $ do
       n <- name
       t <- lift $ map (fromMaybe 0.0) $ runMaybeT $ runTask n
       if t > 0.0 then schedLater (floor t)
                  else done
    work 10

runTask :: forall eff. String -> TaskM eff Number
runTask xs = do

  g <- lift $ getGroup group
  let h = case g of
            Nothing -> ""
            Just (Group g') -> "场景: " <> g'.name <> "\n"

  let t = case g of
            Nothing -> 0.0
            Just (Group g') -> g'.repeat

  m <- MaybeT (getMessage group seq)

  uList <- lift $ getSubscribeList group
  loop (trySend $ sendMessage h m) uList
  rList <- lift $ getRoomSubscribeList group
  loop (trySend $ sendRoomMessage h m) rList

  when (t > 0.0) $ do
    (Milliseconds n) <- liftEff $ map unInstant now
    lift $ updateMessage $ setSchedAt (M.floor (n / 1000.0) + t) m

  pure t

  where ys = split (Pattern "-") xs
        group = unsafePartial $ fromMaybe "" $ ys !! 0
        seq = unsafePartial $ fromMaybe "" $ ys !! 1

loop :: forall eff. (String -> TaskM eff Unit) -> Array String -> TaskM eff Unit
loop f xs
  | null xs = pure unit
  | otherwise = do
  f $ unsafePartial $ fromJust $ head xs
  loop f $ unsafePartial $ fromMaybe [] $ tail xs

trySend :: forall eff. (String -> TaskM eff Unit) -> String -> TaskM eff Unit
trySend f = lift <<< void <<< runMaybeT <<< try <<< f

sendMessage :: forall eff. String -> Message -> String -> TaskM eff Unit
sendMessage g (Message m) n = do
  contact <- MaybeT $ find n
  lift $ runContactM contact $ do
    say $ g <> m.content

sendRoomMessage :: forall eff. String -> Message -> String -> TaskM eff Unit
sendRoomMessage g (Message m) topic = do
  room <- MaybeT $ R.find topic
  lift $ runRoomM room $ do
    R.say $ g <> m.content
