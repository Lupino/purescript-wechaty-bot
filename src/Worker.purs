module Worker
  ( launchWorker
  ) where

import Prelude
import Periodic.Worker (runWorker, addFunc, PERIODIC, work, name, done)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (runAff_, Aff)
import Config (get)
import Data.String (Pattern (..), split)
import DB (Message (..), getMessage, DB, getSubscribeList, getUser, User (..))
import Data.Array ((!!), head, tail, null)
import Data.Either (Either)
import Control.Monad.Eff.Exception (Error)
import Data.Maybe (fromMaybe, fromJust)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Error.Class (try)
import Control.Monad.Maybe.Trans (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Wechaty.Contact (say, find)
import Wechaty.Types (WECHATY, runContactM)

type TaskM eff = MaybeT (Aff (wechaty :: WECHATY, db :: DB | eff))

doError :: forall eff a. Either Error a → Eff eff Unit
doError _ = pure unit

launchWorker :: forall eff. Eff (periodic :: PERIODIC, db :: DB, wechaty :: WECHATY | eff) Unit
launchWorker = do
  runWorker (get "periodic") $ do
    addFunc "send-message" $ do
       n <- name
       liftEff $ runAff_ doError $ void $ runMaybeT $ runSendMessage n
       done
    work 10

runSendMessage :: forall eff. String -> TaskM eff Unit
runSendMessage xs = do
  m <- MaybeT (getMessage group seq)
  uList <- lift $ getSubscribeList group
  loopSendMessage m uList
  where ys = split (Pattern "-") xs
        group = unsafePartial $ fromMaybe "" $ ys !! 0
        seq = unsafePartial $ fromMaybe "" $ ys !! 1

loopSendMessage :: forall eff. Message -> Array String -> TaskM eff Unit
loopSendMessage m xs
  | null xs = pure unit
  | otherwise = do
  trySendMessage m $ unsafePartial $ fromJust $ head xs
  loopSendMessage m $ unsafePartial $ fromMaybe [] $ tail xs

trySendMessage :: forall eff. Message -> String -> TaskM eff Unit
trySendMessage m = lift <<< void <<< runMaybeT <<< try <<< sendMessage m

sendMessage :: forall eff. Message -> String -> TaskM eff Unit
sendMessage (Message m) uid = do
  (User u) <- MaybeT $ getUser uid
  contact <- MaybeT $ find u.name
  lift $ runContactM contact $ do
    say m.content
