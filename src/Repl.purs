module Repl (launchRepl, ReplState, initReplState, checkWhitelist) where

import Prelude

import Effect.Aff (Aff, runAff_)
import Effect (Effect)
import Effect.Console (error)
import Effect.Exception (message)
import Data.Either (Either(..))
import Data.String (trim, drop, length, null, joinWith)
import Node.ReadLine (Interface, createConsoleInterface, setPrompt, setLineHandler, prompt, Completer)
import Utils (startsWith)
import Wechaty.Contact (findAll, getContactName, runContactT, say, Contact, self)
import Wechaty.Room (findAll, say) as R
import Wechaty.Room (Room, getRoomTopic, runRoomT)
import Control.Monad.Trans.Class (lift)
import Effect.Ref (Ref, new, read, modify)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Data.Array (elem, (:), delete, filter)
import Data.Array (null) as A

type Whitelist = Array String

data ReplState = IsContact Whitelist Interface Contact
               | IsRoom Whitelist Interface Room
               | IsManager Whitelist Interface Contact
               | Only Whitelist Interface

type Repl a = ReaderT (Ref ReplState) Effect a

runRepl :: forall a. Ref ReplState -> Repl a -> Effect a
runRepl s m = runReaderT m s

initReplState :: Effect (Ref ReplState)
initReplState = do
  rl <- createConsoleInterface completion
  new $ Only [] rl

get :: Repl ReplState
get = do
  ref <- ask
  lift $ read ref

setContactPrompt :: Contact -> Interface -> Effect Unit
setContactPrompt c = setPrompt ps (length ps)
  where ps = "Contact<<" <> getContactName c <> ">> "

setRoomPrompt :: Room -> Interface -> Effect Unit
setRoomPrompt r = setPrompt ps (length ps)
  where ps = "Room<<" <> getRoomTopic r <> ">> "

getInterface :: ReplState -> Interface
getInterface (IsContact _ rl _) = rl
getInterface (IsRoom _ rl _) = rl
getInterface (IsManager _ rl _) = rl
getInterface (Only _ rl) = rl

getWhitelist :: ReplState -> Whitelist
getWhitelist (IsContact w _ _) = w
getWhitelist (IsRoom w _ _) = w
getWhitelist (IsManager w _ _) = w
getWhitelist (Only w _) = w

putWhitelist :: Whitelist -> ReplState -> ReplState
putWhitelist wl (IsContact _ rl c) = IsContact wl rl c
putWhitelist wl (IsRoom _ rl c) = IsRoom wl rl c
putWhitelist wl (IsManager _ rl c) = IsManager wl rl c
putWhitelist wl (Only _ rl) = Only wl rl

addWhitelist :: String -> Whitelist -> Whitelist
addWhitelist xs wl | elem xs wl = wl
                   | otherwise = xs : wl

removeWhitelist :: String -> Whitelist -> Whitelist
removeWhitelist xs = delete xs

clearWhitelist :: String -> Whitelist -> Whitelist
clearWhitelist _ _ = []

replaceWhitelist :: (String -> Whitelist -> Whitelist) -> String -> Repl Unit
replaceWhitelist f xs = do
  ref <- ask
  void $ lift $ modify (\ps -> putWhitelist (f xs (getWhitelist ps)) ps) ref

switch
  :: forall a. (a -> Interface -> Effect Unit)
  -> (Whitelist -> Interface -> a -> ReplState)
  -> a -> Repl Unit
switch p f a = do
  ref <- ask
  void $ lift $ modify (\ps -> f (getWhitelist ps) (getInterface ps) a) ref
  lift $ p a =<< map getInterface (read ref)

switchContact :: Contact -> Repl Unit
switchContact = switch setContactPrompt IsContact

switchManager :: Repl Unit
switchManager = do
  s <- lift self
  switch setContactPrompt IsManager s

switchRoom :: Room -> Repl Unit
switchRoom room = switch setRoomPrompt IsRoom room

data Cmd =
    FindContact String
  | FindRoom String
  | ShowWhitelist
  | AddWhitelist String
  | RemoveWhitelist String
  | ClearWhitelist
  | Msg String
  | Exit
  | Help
  | Empty

parseCmd :: String -> Cmd
parseCmd xs
  | startsWith xs ".contact" = FindContact $ trim $ drop 8 xs
  | startsWith xs ".room" = FindRoom $ trim $ drop 5 xs
  | startsWith xs ".whitelist add" = AddWhitelist $ trim $ drop 14 xs
  | startsWith xs ".whitelist remove" = RemoveWhitelist $ trim $ drop 17 xs
  | startsWith xs ".whitelist clear" = ClearWhitelist
  | startsWith xs ".whitelist" = ShowWhitelist
  | startsWith xs ".exit" = Exit
  | startsWith xs ".help" = Help
  | null xs = Empty
  | otherwise = Msg xs

hits :: Array String
hits =
  [ ".contact"
  , ".room"
  , ".whitelist"
  , ".whitelist add"
  , ".whitelist remove"
  , ".whitelist clear"
  , ".exit"
  , ".help"
  ]

completion :: Completer
completion s = pure { completions: filter (flip startsWith s) hits, matched: s }

help :: Array String
help =
  [ ".contact CONTACT          -- 切换用户"
  , ".room ROOM                -- 切换聊天群"
  , ".whitelist                -- 显示白名单"
  , ".whitelist add STRING     -- 添加显示白名单"
  , ".whitelist remove STRING  -- 移除显示白名单"
  , ".whitelist clear          -- 清空显示白名单"
  , ".exit                     -- 退出聊天, 跟自己聊"
  , ".help                     -- 显示本帮助"
  ]

mkLineHandler
  :: Ref ReplState -> (Cmd -> Repl Unit)
  -> String -> Effect Unit
mkLineHandler ps f = runRepl ps <<< f <<< parseCmd

handlers :: Cmd -> Repl Unit
handlers (FindContact n) = do
  ref <- ask
  ps <- get
  lift $ flip runAff_ (findAll n) $ \r -> do
    case r of
      (Left e) -> error $ "Error: " <> message e
      (Right []) -> error $ "Contact<<" <> n <> ">> Not Found."
      (Right [c]) -> runRepl ref $ switchContact c
      (Right xs) ->  error $ "Found Contact:\n" <> (joinWith "\n" $ map getContactName xs)
    showPrompt ps
handlers (FindRoom n) = do
  ref <- ask
  ps <- get
  lift $ flip runAff_ (R.findAll n) $ \r -> do
    case r of
      (Left e) -> error $ "Error: " <> message e
      (Right []) ->  error $ "Room<<" <> n <> ">> Not Found."
      (Right [c]) -> runRepl ref $ switchRoom c
      (Right xs) ->  error $ "Found Room:\n" <> (joinWith "\n" $ map getRoomTopic xs)
    showPrompt ps

handlers (Msg m) = do
  ps <- get
  lift $ flip runAff_ (sendMessage ps m) $ \r -> do
    case r of
      (Left e) -> error $ "Error: " <> message e
      (Right _) -> pure unit
    showPrompt ps

handlers (AddWhitelist xs) = replaceWhitelist addWhitelist xs *> showPrompt_
handlers (RemoveWhitelist xs) = replaceWhitelist removeWhitelist xs *> showPrompt_
handlers ClearWhitelist = replaceWhitelist clearWhitelist "" *> showPrompt_
handlers ShowWhitelist = do
  wl <- getWhitelist <$> get
  lift $ error $ "Whitelist:\n" <> joinWith "\n" wl

handlers Exit = switchManager *> showPrompt_

handlers Help = do
  lift $ error $ "\n" <> joinWith "\n" help
  showPrompt_

handlers Empty = showPrompt_

sendMessage :: ReplState -> String -> Aff Unit
sendMessage (IsContact _ _ c) = runContactT c <<< say
sendMessage (IsRoom _ _ c) = runRoomT c <<< R.say
sendMessage (IsManager _ _ c) = runContactT c <<< say
sendMessage (Only _ _) = \_ -> pure unit

launchRepl :: Ref ReplState -> Effect Unit
launchRepl ref = do
  runRepl ref switchManager
  ps <- read ref
  setLineHandler (getInterface ps) (mkLineHandler ref handlers)
  showPrompt ps

showPrompt :: ReplState -> Effect Unit
showPrompt = prompt <<< getInterface

showPrompt_ :: Repl Unit
showPrompt_ = lift <<< showPrompt =<< get

checkWhitelist :: Ref ReplState -> String -> Effect Unit -> Effect Unit
checkWhitelist ref h io = do
  wl <- getWhitelist <$> read ref
  if A.null wl then io
    else if elem h wl then io else pure unit
