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
           getGroup, Group (..))
import Data.Array ((!!), head, tail, null)
import Data.Either (Either)
import Data.Maybe (fromMaybe, fromJust, Maybe (..))
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

  g <- lift $ getGroup group
  let h = case g of
            Nothing -> ""
            Just (Group g') -> "场景: " <> g'.name <> "\n"

  m <- MaybeT (getMessage group seq)

  uList <- lift $ getSubscribeList group
  loop (trySend $ sendMessage h m) uList
  rList <- lift $ getRoomSubscribeList group
  loop (trySend $ sendRoomMessage h m) rList
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
    R.say $ "\n" <> g <> m.content
