module Main where

import Prelude

import Chat (launchChat)
import Chat as Chat
import Config (periodicHost)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import DB (messageMod)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim, drop)
import Database.Sequelize (sync)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Periodic.Client (newClient)
import Plan.Trans (runPlanT, initRouteRef, PlanT, reply)
import Repl (launchRepl, initReplState, checkWhitelist)
import Utils (startsWith)
import Wechaty (initWechaty, onScan, showQrcode, onLogin, onMessage, start, onError, runWechatyT)
import Wechaty.Contact (say, getContactName, ContactT, Contact)
import Wechaty.Message (handleContact, handleRoom, room, self, from, content)
import Wechaty.Room (getRoomTopic, RoomT, sayTo)
import Worker (launchWorker)


type ContactHandler m = ContactT (PlanT Chat.Options String m)

type RoomHandler m = RoomT (PlanT Chat.Options String m)

contactHandler
  :: forall m. MonadAff m
  => String -> ContactHandler m Unit
contactHandler xs = do
  c <- ask
  ret <- lift $ reply (Chat.Contact c) xs
  case ret of
    Left _ -> pure unit
    Right m -> say m

roomHandler
  :: forall m0. MonadAff m0
  => Contact -> Boolean -> String -> RoomHandler m0 Unit
roomHandler contact manager xs = do
  r <- ask
  go $ \m0 -> do
    ret <- lift $ reply (Chat.Room r manager) m0
    case ret of
      Left _ -> pure unit
      Right m -> sayTo contact m

  where go :: forall m. Monad m => (String -> RoomHandler m Unit) -> RoomHandler m Unit
        go f | startsWith xs "@小云" = f $ trim $ drop 3 xs
             | startsWith xs "@机器人" = f $ trim $ drop 4 xs
             | startsWith xs "@robot" = f $ trim $ drop 6 xs
             | startsWith xs "@xiaoyun" = f $ trim $ drop 8 xs
             | otherwise = pure unit

handleScan :: String -> Int -> Effect Unit
handleScan url 200 = pure unit
handleScan url 201 = pure unit
handleScan url _ = showQrcode url

main :: Effect Unit
main = do
  client <- newClient periodicHost {max: 10}
  ps <- initReplState client
  bot <- initWechaty
  routeRef <- initRouteRef
  launchAff_ $ do
    sync messageMod {force: false}
    runPlanT routeRef $ do
      launchChat
      runWechatyT (launchAff_ <<< runPlanT routeRef) bot $ do
        onScan $ \url code -> do
          handleScan url code
          log url
          log $ "[" <> show code <> "] Scan QR Code above url to log in:"
        onLogin $ do
          liftEffect $ log "Logined"
          say "欢迎小主人归来"
          liftEffect $ launchRepl ps
          liftAff launchWorker
        onMessage $ do
          r <- room
          s <- self
          c <- from
          msg <- content
          case r of
            Nothing -> do
              liftEffect
                $ checkWhitelist ps (getContactName c)
                $ error $ "From<<" <> getContactName c <> ">>: " <> msg
              handleContact contactHandler
            Just r0 -> do
              liftEffect
                $ checkWhitelist ps (getRoomTopic r0)
                $ error $ "Room<<" <> getRoomTopic r0 <> ">><<" <> getContactName c <> ">>: " <> msg
              handleRoom r0 s $ roomHandler

        start
        onError $ \msg -> log $ "error: " <> msg
