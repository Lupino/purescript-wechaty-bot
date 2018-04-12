module Repl (launchRepl, ReplState, initReplState, showPrompt) where

import Prelude

import Control.Monad.Aff (Aff, runAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, message)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (trim, drop, length, null)
import Node.ReadLine (Interface, READLINE, createConsoleInterface, noCompletion, setPrompt, setLineHandler, prompt)
import Utils (startsWith)
import Wechaty.Contact (find, getContactName, runContactT, say, Contact, self)
import Wechaty.Room (find, say) as R
import Wechaty.Room (Room, getRoomTopic, runRoomT)
import Wechaty.Types (WECHATY)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, writeRef)
import Control.Monad.Reader (ReaderT, runReaderT, ask)

data ReplState = IsContact Interface Contact | IsRoom Interface Room | IsManager Interface Contact | Only Interface

type Repl eff a = ReaderT (Ref ReplState) (Eff (ref :: REF, readline:: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff)) a

runRepl :: forall eff a. Ref ReplState -> Repl eff a -> Eff (ref :: REF, readline:: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff) a
runRepl s m = runReaderT m s

initReplState :: forall eff. Eff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff) ReplState
initReplState = do
  rl <- createConsoleInterface noCompletion
  pure $ Only rl

get :: forall eff. Repl eff ReplState
get = do
  ref <- ask
  lift $ readRef ref

put :: forall eff. ReplState -> Repl eff Unit
put ps = do
  ref <- ask
  lift $ writeRef ref ps

setContactPrompt :: forall eff. Contact -> Interface -> Eff (readline :: READLINE | eff) Unit
setContactPrompt c = setPrompt ps (length ps)
  where ps = "Contact<<" <> getContactName c <> ">> "

setRoomPrompt :: forall eff. Room -> Interface -> Eff (readline :: READLINE | eff) Unit
setRoomPrompt r = setPrompt ps (length ps)
  where ps = "Room<<" <> getRoomTopic r <> ">> "

getInterface :: ReplState -> Interface
getInterface (IsContact rl _) = rl
getInterface (IsRoom rl _) = rl
getInterface (IsManager rl _) = rl
getInterface (Only rl) = rl

switchContact :: forall eff. Contact -> Repl eff Unit
switchContact c = do
  rl <- getInterface <$> get
  lift $ setContactPrompt c rl
  put (IsContact rl c)

switchManager :: forall eff. Repl eff Unit
switchManager = do
  s <- lift self
  rl <- getInterface <$> get
  lift $ setContactPrompt s rl
  put (IsManager rl s)

switchRoom :: forall eff. Room -> Repl eff Unit
switchRoom room = do
  rl <- getInterface <$> get
  lift $ setRoomPrompt room rl
  put (IsRoom rl room)

data Cmd =
    FindContact String
  | FindRoom String
  | Msg String
  | Exit
  | Help
  | Empty

parseCmd :: String -> Cmd
parseCmd xs
  | startsWith xs ".contact" = FindContact $ trim $ drop 8 xs
  | startsWith xs ".room" = FindRoom $ trim $ drop 5 xs
  | startsWith xs ".exit" = Exit
  | startsWith xs ".help" = Help
  | null xs = Empty
  | otherwise = Msg xs

mkLineHandler
  :: forall eff. Ref ReplState -> (Cmd -> Repl eff Unit)
  -> String -> Eff (ref :: REF, readline:: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff) Unit
mkLineHandler ps f = runRepl ps <<< f <<< parseCmd

handlers :: forall eff. Cmd -> Repl eff Unit
handlers (FindContact n) = do
  ref <- ask
  ps <- get
  lift $ flip runAff_ (find n) $ \r -> do
    case r of
      (Left e) -> log $ "Error: " <> message e
      (Right (Just c)) -> runRepl ref $ switchContact c
      (Right Nothing) ->  log $ "Contact<<" <> n <> ">> Not Found."
    showPrompt ps
handlers (FindRoom n) = do
  ref <- ask
  ps <- get
  lift $ flip runAff_ (R.find n) $ \r -> do
    case r of
      (Left e) -> log $ "Error: " <> message e
      (Right (Just c)) -> runRepl ref $ switchRoom c
      (Right Nothing) ->  log $ "Room<<" <> n <> ">> Not Found."
    showPrompt ps
handlers (Msg m) = do
  ps <- get
  lift $ flip runAff_ (sendMessage ps m) $ \r -> do
    case r of
      (Left e) -> log $ "Error: " <> message e
      (Right _) -> pure unit
    showPrompt ps

handlers Exit = do
  switchManager
  ps <- get
  lift $ showPrompt ps

handlers _ = do
  ps <- get
  lift $ showPrompt ps

sendMessage :: forall eff. ReplState -> String -> Aff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff) Unit
sendMessage (IsContact _ c) = runContactT c <<< say
sendMessage (IsRoom _ c) = runRoomT c <<< R.say
sendMessage (IsManager _ c) = runContactT c <<< say
sendMessage (Only _) = \_ -> pure unit

launchRepl :: forall eff. ReplState -> Eff (ref :: REF, readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff) Unit
launchRepl ps = do
  ref <- newRef ps
  runRepl ref switchManager
  setLineHandler (getInterface ps) (mkLineHandler ref handlers)

showPrompt :: forall eff. ReplState -> Eff (readline :: READLINE, console :: CONSOLE, exception :: EXCEPTION, wechaty :: WECHATY | eff) Unit
showPrompt = prompt <<< getInterface
