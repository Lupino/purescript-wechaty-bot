module Main where

import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff (Eff)
import Prelude
import Wechaty (initWechaty, onLogin, onMessage, start)
import Wechaty.Types (ContactM, WECHATY, runWechatyM)
import Wechaty.Contact (say)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Wechaty.Message (content, self, handleContact, handleContact_)
import Data.String.Regex (test, regex, Regex)
import Data.String.Regex.Flags (noFlags, ignoreCase)
import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)

selfHandler :: forall eff. String -> ContactM eff Unit
selfHandler "ping" = say "pong"
selfHandler _ = pure unit

reHere :: Regex
reHere = unsafePartial $ fromRight $ regex "^在.*" noFlags

reHi :: Regex
reHi = unsafePartial $ fromRight $ regex "^(hi.*|你好.*)" ignoreCase

contactHandler :: forall eff. String -> ContactM eff Unit
contactHandler "ping" = say "pong"
contactHandler msg | test reHere msg = say "我家小主人在海外云游"
                   | test reHi msg   = say "小云这厢有礼了"
                   | otherwise = pure unit

main :: Eff (console :: CONSOLE, wechaty :: WECHATY) Unit
main = do
  bot <- initWechaty
  launchAff_ $
    runWechatyM bot $ do
      onLogin $ do
        liftEff $ log "Logined"
        say "欢迎小主人归来"
      onMessage $ do
        msg <- content
        liftEff $ log msg
        s <- self
        if s then handleContact_ selfHandler
             else handleContact contactHandler
      start
