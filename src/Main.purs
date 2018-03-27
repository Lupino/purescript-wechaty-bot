module Main where

import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Prelude
import Wechaty (initWechaty, onLogin, onMessage, start)
import Wechaty.Types (WECHATY, runWechatyM)
import Wechaty.Contact (say)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Wechaty.Message (self, handleContact, handleContact_)
import DB (DB)

import Robot (subscriberHandler, managerHandler)

main :: Eff (console :: CONSOLE, wechaty :: WECHATY, db :: DB) Unit
main = do
  bot <- initWechaty
  launchAff_ $
    runWechatyM bot $ do
      onLogin $ do
        liftEff $ log "Logined"
        say "欢迎小主人归来"
      onMessage $ do
        s <- self
        if s then handleContact_ managerHandler
             else handleContact subscriberHandler
      start
