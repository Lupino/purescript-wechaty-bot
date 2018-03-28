module Main where

import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Prelude
import Wechaty (initWechaty, onScan, showQrcode, onLogin, onMessage, start, onError)
import Wechaty.Types (WECHATY, runWechatyM)
import Wechaty.Contact (say)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Wechaty.Message (self, handleContact, handleContact_)
import DB (DB)

import Robot (subscriberHandler, managerHandler)
import Config (get)
import Periodic.Client (PERIODIC, newClient)
import Worker (launchWorker)

handleScan :: forall eff. String -> Int -> Eff eff Unit
handleScan url 200 = showQrcode url
handleScan url 201 = showQrcode url
handleScan _ _ = pure unit

main :: Eff (console :: CONSOLE, wechaty :: WECHATY, db :: DB, periodic :: PERIODIC) Unit
main = do
  bot <- initWechaty
  client <- newClient (get "periodic") {max: 10}
  launchAff_ $
    runWechatyM bot $ do
      onScan $ \url code -> do
        handleScan url code
        log url
        log $ "[" <> show code <> "] Scan QR Code above url to log in:"
      onLogin $ do
        liftEff $ log "Logined"
        say "欢迎小主人归来"
      onMessage $ do
        s <- self
        if s then handleContact_ $ managerHandler client
             else handleContact subscriberHandler
      start
      onError $ \msg -> log $ "error: " <> msg

  launchWorker
