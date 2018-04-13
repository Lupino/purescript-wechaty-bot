module Main where

import Prelude

import Config (get)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Now (NOW)
import DB (DB)
import Data.Maybe (Maybe(..))
import Periodic.Client (PERIODIC, newClient)
import Robot (managerHandler, subscriberHandler, roomSubscriberHandler)
import Wechaty (initWechaty, onScan, showQrcode, onLogin, onMessage, start, onError, runWechatyT)
import Wechaty.Contact (say, getContactName)
import Wechaty.Room (getRoomTopic)
import Wechaty.Message (handleContact, handleRoom, room, self, from, content)
import Wechaty.Types (WECHATY)
import Control.Monad.Eff.Ref (REF)
import Plan.Trans (runPlanT, initRouteRef)
import Worker (launchWorker)
import Chatter (launchChatter)
import Node.ReadLine (READLINE)
import Repl (launchRepl, initReplState, showPrompt)
import Control.Monad.Eff.Exception (EXCEPTION)

handleScan :: forall eff. String -> Int -> Eff eff Unit
handleScan url 200 = pure unit
handleScan url 201 = pure unit
handleScan url _ = showQrcode url

main :: Eff (console :: CONSOLE, wechaty :: WECHATY, db :: DB, periodic :: PERIODIC, now :: NOW, ref :: REF, readline :: READLINE, exception :: EXCEPTION) Unit
main = do
  ps <- initReplState
  bot <- initWechaty
  client <- newClient (get "periodic") {max: 10}
  routeRef <- initRouteRef
  launchAff_ $ do
    runPlanT routeRef $ do
      launchChatter
      runWechatyT (launchAff_ <<< runPlanT routeRef) bot $ do
        onScan $ \url code -> do
          handleScan url code
          log url
          log $ "[" <> show code <> "] Scan QR Code above url to log in:"
        onLogin $ do
          liftEff $ log "Logined"
          say "欢迎小主人归来"
          liftEff $ launchRepl ps
          liftEff $ showPrompt ps
        onMessage $ do
          r <- room
          s <- self
          c <- from
          msg <- content
          case r of
            Nothing ->
              if s then handleContact $ managerHandler client
                   else do
                      liftEff $ log $ "\nContact<<" <> getContactName c <> ">>: " <> msg
                      handleContact $ subscriberHandler
            Just r0 -> do
              liftEff $ log $ "\nRoom<<" <> getRoomTopic r0 <> ">><<" <> getContactName c <> ">>: " <> msg
              handleRoom r0 s $ roomSubscriberHandler

        start
        onError $ \msg -> log $ "error: " <> msg

    launchWorker
