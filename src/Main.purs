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
import Wechaty.Contact (say)
import Wechaty.Message (handleContact, handleRoom, room, self)
import Wechaty.Types (WECHATY)
import Control.Monad.Eff.Ref (REF)
import Plan.Trans (runPlanT, initRouteRef)
import Worker (launchWorker)

handleScan :: forall eff. String -> Int -> Eff eff Unit
handleScan url 200 = pure unit
handleScan url 201 = pure unit
handleScan url _ = showQrcode url

main :: Eff (console :: CONSOLE, wechaty :: WECHATY, db :: DB, periodic :: PERIODIC, now :: NOW, ref :: REF) Unit
main = do
  bot <- initWechaty
  client <- newClient (get "periodic") {max: 10}
  routeRef <- initRouteRef
  launchAff_ $ do
    runPlanT routeRef $ do
      runWechatyT (launchAff_ <<< runPlanT routeRef) bot $ do
        onScan $ \url code -> do
          handleScan url code
          log url
          log $ "[" <> show code <> "] Scan QR Code above url to log in:"
        onLogin $ do
          liftEff $ log "Logined"
          say "欢迎小主人归来"
        onMessage $ do
          r <- room
          s <- self
          case r of
            Nothing -> if s then handleContact $ managerHandler client
                            else handleContact $ subscriberHandler
            Just r0 -> handleRoom r0 s $ roomSubscriberHandler

        start
        onError $ \msg -> log $ "error: " <> msg

    launchWorker
