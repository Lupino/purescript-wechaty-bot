module Worker
  ( launchWorker
  ) where

import Prelude

import Config (periodicHost)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import DB (Message (..), messageMod)
import Data.Dayjs (fromUnixTime, toUnixTime)
import Data.Maybe (fromMaybe, Maybe (..))
import Data.String (drop)
import Database.Sequelize (findOne)
import Effect.Aff (Aff, launchAff_, try)
import Periodic.Worker (addFunc, done, name, runWorkerT, schedLater, work)
import Utils (adjustTime, startsWith)
import Wechaty.Contact (say, runContactT, name) as C
import Wechaty (runWechatyT, findContact, findRoom, userSelf, Wechaty)
import Wechaty.Room (say, runRoomT) as R

type TaskM = MaybeT Aff

launchWorker :: Wechaty -> Aff Unit
launchWorker bot = do
  runWorkerT launchAff_ periodicHost $ do
    addFunc "send-message" $ do
       n <- name
       t <- lift $ map (fromMaybe 0) $ runMaybeT $ runTask bot n
       if t > 0 then schedLater t
                else done
    work 10

runTask :: Wechaty -> String -> TaskM Int
runTask bot xs = do
  (Message t) <- MaybeT $ findOne messageMod {where: {id: xs}}
  go t.user (Message t)

  pure $ (toUnixTime $ adjustTime t.repeat $ fromUnixTime t.sched_at) - t.sched_at
  where go :: String -> Message -> TaskM Unit
        go u t | startsWith u "user-" = trySend (sendMessage bot t) $ drop 5 u
               | startsWith u "room-" = trySend (sendRoomMessage bot t) $ drop 5 u
               | otherwise = pure unit

trySend :: (String -> TaskM Unit) -> String -> TaskM Unit
trySend f = lift <<< void <<< runMaybeT <<< try <<< f

sendMessage :: Wechaty -> Message -> String -> TaskM Unit
sendMessage bot (Message m) n = lift $ do
  r <- runWechatyT launchAff_ bot $ findContact n
  case r of
    Nothing -> do
      s <- runWechatyT launchAff_ bot userSelf
      n0 <- C.runContactT s C.name
      if n0 == n then C.runContactT s $ C.say m.message
        else pure unit
    Just c -> C.runContactT c $ C.say m.message

sendRoomMessage :: Wechaty -> Message -> String -> TaskM Unit
sendRoomMessage bot (Message m) topic = do
  room <- MaybeT $ runWechatyT launchAff_ bot $ findRoom topic
  lift $ R.runRoomT room $ R.say $ m.message
