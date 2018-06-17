module Worker
  ( launchWorker
  ) where

import Prelude

import Config (periodicHost)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import DB (MessageType, messageMod)
import Data.Dayjs (fromUnixTime, toUnixTime)
import Data.Maybe (fromMaybe)
import Data.String (drop)
import Database.Sequelize (findOne)
import Effect.Aff (Aff, launchAff_, try)
import Periodic.Worker (addFunc, done, name, runWorkerT, schedLater, work)
import Utils (adjustTime, startsWith)
import Wechaty.Contact (say, find, runContactT)
import Wechaty.Room as R

type TaskM = MaybeT Aff

launchWorker :: Aff Unit
launchWorker = do
  runWorkerT launchAff_ periodicHost $ do
    addFunc "send-message" $ do
       n <- name
       t <- lift $ map (fromMaybe 0) $ runMaybeT $ runTask n
       if t > 0 then schedLater t
                else done
    work 10

runTask :: String -> TaskM Int
runTask xs = do
  t <- MaybeT $ findOne messageMod {where: {id: xs}}
  go t.user t

  pure $ (toUnixTime $ adjustTime t.repeat $ fromUnixTime t.sched_at) - t.sched_at
  where go :: String -> MessageType -> TaskM Unit
        go u t | startsWith u "user-" = trySend (sendMessage t) $ drop 5 u
               | startsWith u "room-" = trySend (sendRoomMessage t) $ drop 5 u
               | otherwise = pure unit

trySend :: (String -> TaskM Unit) -> String -> TaskM Unit
trySend f = lift <<< void <<< runMaybeT <<< try <<< f

sendMessage :: MessageType -> String -> TaskM Unit
sendMessage m n = do
  contact <- MaybeT $ find n
  lift $ runContactT contact $ do
    say $ m.message

sendRoomMessage :: MessageType -> String -> TaskM Unit
sendRoomMessage m topic = do
  room <- MaybeT $ R.find topic
  lift $ R.runRoomT room $ do
    R.say $ m.message
