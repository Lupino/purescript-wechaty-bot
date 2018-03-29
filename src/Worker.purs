module Worker
  ( launchWorker
  ) where

import Prelude

import Config (get)
import Control.Monad.Aff (runAff_, Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Error.Class (try)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import DB (Message(..), getMessage, DB, getSubscribeList, getRoomSubscribeList,
           getUser, User(..), getRoom, Room (..))
import Data.Array ((!!), head, tail, null)
import Data.Either (Either)
import Data.Maybe (fromMaybe, fromJust)
import Data.String (Pattern(..), split)
import Partial.Unsafe (unsafePartial)
import Periodic.Worker (runWorker, addFunc, PERIODIC, work, name, done)
import Wechaty.Contact (say, find)
import Wechaty.Room as R
import Wechaty.Types (WECHATY, runContactM, runRoomM)

type TaskM eff = MaybeT (Aff (wechaty :: WECHATY, db :: DB | eff))

doError :: forall eff a. Either Error a → Eff eff Unit
doError _ = pure unit

launchWorker :: forall eff. Eff (periodic :: PERIODIC, db :: DB, wechaty :: WECHATY | eff) Unit
launchWorker = do
  runWorker (get "periodic") $ do
    addFunc "send-message" $ do
       n <- name
       liftEff $ runAff_ doError $ void $ runMaybeT $ runTask n
       done
    work 10

runTask :: forall eff. String -> TaskM eff Unit
runTask xs = do
  m <- MaybeT (getMessage group seq)
  uList <- lift $ getSubscribeList group
  loop (trySend $ sendMessage m) uList
  rList <- lift $ getRoomSubscribeList group
  loop (trySend $ sendRoomMessage m) rList
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

sendMessage :: forall eff. Message -> String -> TaskM eff Unit
sendMessage (Message m) uid = do
  (User u) <- MaybeT $ getUser uid
  contact <- MaybeT $ find u.name
  lift $ runContactM contact $ do
    say m.content

sendRoomMessage :: forall eff. Message -> String -> TaskM eff Unit
sendRoomMessage (Message m) rid = do
  (Room u) <- MaybeT $ getRoom rid
  room <- MaybeT $ R.find u.topic
  lift $ runRoomM room $ do
    R.say m.content
