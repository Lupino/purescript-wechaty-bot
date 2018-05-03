module Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, error, CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Wechaty (initWechaty, onScan, showQrcode, onLogin, onMessage, start, onError, runWechatyT)
import Wechaty.Contact (say, getContactName, ContactT, Contact)
import Wechaty.Room (getRoomTopic, RoomT, sayTo)
import Wechaty.Message (handleContact, handleRoom, room, self, from, content)
import Wechaty.Types (WECHATY)
import Control.Monad.Eff.Ref (REF)
import Plan.Trans (runPlanT, initRouteRef, PlanT, reply)
import Chatter (launchChatter)
import Node.ReadLine (READLINE)
import Repl (launchRepl, initReplState, checkWhitelist)
import Control.Monad.Eff.Exception (EXCEPTION)
import Utils (startsWith)
import Data.String (trim, drop)


type ContactHandler m = ContactT (PlanT Unit String m)

type RoomHandler m = RoomT (PlanT Unit String m)

contactHandler
  :: forall m eff. MonadAff (wechaty :: WECHATY, ref :: REF | eff) m
  => String -> ContactHandler m Unit
contactHandler xs = do
  ret <- lift $ reply unit xs
  case ret of
    Left _ -> pure unit
    Right m -> say m

roomHandler
  :: forall m0 eff. MonadAff (wechaty :: WECHATY, ref :: REF | eff) m0
  => Contact -> Boolean -> String -> RoomHandler m0 Unit
roomHandler contact manager xs =
  go $ \m0 -> do
    ret <- lift $ reply unit m0
    case ret of
      Left _ -> pure unit
      Right m -> sayTo contact m

  where go :: forall m. Monad m => (String -> RoomHandler m Unit) -> RoomHandler m Unit
        go f | startsWith xs "@小云" = f $ trim $ drop 3 xs
             | startsWith xs "@机器人" = f $ trim $ drop 4 xs
             | startsWith xs "@robot" = f $ trim $ drop 6 xs
             | startsWith xs "@xiaoyun" = f $ trim $ drop 8 xs
             | otherwise = pure unit

handleScan :: forall eff. String -> Int -> Eff eff Unit
handleScan url 200 = pure unit
handleScan url 201 = pure unit
handleScan url _ = showQrcode url

main :: Eff (console :: CONSOLE, wechaty :: WECHATY, now :: NOW, ref :: REF, readline :: READLINE, exception :: EXCEPTION) Unit
main = do
  ps <- initReplState
  bot <- initWechaty
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
        onMessage $ do
          r <- room
          s <- self
          c <- from
          msg <- content
          case r of
            Nothing -> do
              liftEff
                $ checkWhitelist ps (getContactName c)
                $ error $ "From<<" <> getContactName c <> ">>: " <> msg
              handleContact contactHandler
            Just r0 -> do
              liftEff
                $ checkWhitelist ps (getRoomTopic r0)
                $ error $ "Room<<" <> getRoomTopic r0 <> ">><<" <> getContactName c <> ">>: " <> msg
              handleRoom r0 s $ roomHandler

        start
        onError $ \msg -> log $ "error: " <> msg
