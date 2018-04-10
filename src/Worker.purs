module Worker
  ( launchWorker
  ) where

import Prelude

import Config (get)
import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Error.Class (try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import DB (DB, Group(..), Message, getGroup, getMessage, getRoomSubscribeList, getSubscribeList, setSchedAt, updateMessage, getContent, getSchedAt)
import Data.Array ((!!), head, tail, null)
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)
import Periodic.Worker (PERIODIC, addFunc, done, name, runWorkerT, schedLater, work)
import Utils (getTimeStamp)
import Wechaty.Contact (say, find, runContactT)
import Wechaty.Room as R
import Wechaty.Types (WECHATY)

type TaskM eff = MaybeT (Aff (wechaty :: WECHATY, db :: DB, now :: NOW | eff))

threshold :: Number
threshold = 30.0 * 60.0

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

  now <- liftEff getTimeStamp

  when (getSchedAt m > now - threshold) $ do
    uList <- lift $ getSubscribeList group
    loop (trySend $ sendMessage h m) uList
    rList <- lift $ getRoomSubscribeList group
    loop (trySend $ sendRoomMessage h m) rList

  if t > 0.0 then do
    let nextT = nextSched (getSchedAt m) t now
    lift $ updateMessage $ setSchedAt nextT m
    pure $ nextT - now
    else pure 0.0

  where ys = split (Pattern "-") xs
        group = unsafePartial $ fromMaybe "" $ ys !! 0
        seq = unsafePartial $ fromMaybe "" $ ys !! 1
        nextSched :: Number -> Number -> Number -> Number
        nextSched next t now | next > now = next
                             | otherwise = nextSched (next + t) t now

loop :: forall eff. (String -> TaskM eff Unit) -> Array String -> TaskM eff Unit
loop f xs
  | null xs = pure unit
  | otherwise = do
  f $ unsafePartial $ fromJust $ head xs
  loop f $ unsafePartial $ fromMaybe [] $ tail xs

trySend :: forall eff. (String -> TaskM eff Unit) -> String -> TaskM eff Unit
trySend f = lift <<< void <<< runMaybeT <<< try <<< f

sendMessage :: forall eff. String -> Message -> String -> TaskM eff Unit
sendMessage g m n = do
  contact <- MaybeT $ find n
  lift $ runContactT contact $ do
    say $ g <> getContent m

sendRoomMessage :: forall eff. String -> Message -> String -> TaskM eff Unit
sendRoomMessage g m topic = do
  room <- MaybeT $ R.find topic
  lift $ R.runRoomT room $ do
    R.say $ g <> getContent m
