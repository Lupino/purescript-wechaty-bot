module Repl
  ( launchRepl
  , ReplState
  , initReplState
  , checkWhitelist
  , Select
  , StateType
  , Whitelist
  ) where

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
import Data.Array (elem, (:), delete, filter, mapWithIndex, (!!))
import Data.Array (null) as A
import Data.Maybe (Maybe (..))
import Data.Int (fromString)

type Whitelist = Array String

data StateType = IsContact Contact
               | IsRoom Room
               | IsManager Contact
               | IsEmpty

data Select = SelectRoom (Array Room)
            | SelectContact (Array Contact)

type ReplState =
  { whitelist :: Whitelist
  , interface :: Interface
  , select :: Maybe Select
  , state :: StateType
  }

type Repl a = ReaderT (Ref ReplState) Effect a

runRepl :: forall a. Ref ReplState -> Repl a -> Effect a
runRepl s m = runReaderT m s

initReplState :: Effect (Ref ReplState)
initReplState = do
  rl <- createConsoleInterface completion
  new {whitelist: [], interface: rl, select: Nothing, state: IsEmpty}

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
  void $ lift $ modify (\ps -> ps {whitelist = f xs ps.whitelist}) ref

adjustSelect :: Maybe Select -> Repl Unit
adjustSelect xs = do
  ref <- ask
  void $ lift $ modify (\ps -> ps {select = xs}) ref

switch :: forall a. (a -> Interface -> Effect Unit) -> (a -> StateType) -> a -> Repl Unit
switch p f a = do
  ref <- ask
  ps <- lift $ modify (\rs -> rs {state = f a}) ref
  lift $ p a ps.interface

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
  | Select String
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
  | startsWith xs ".select" = Select $ trim $ drop 7 xs
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
  , ".select"
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
  , ".select INT               -- 选择用户或聊天群"
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
      (Right xs) -> do
         error $ "Found Contact:\n"
               <> (joinWith "\n" $ mapWithIndex (\i c -> show i <> ". " <> getContactName c) xs)
               <> "\n.select INT 选择用户"
         runRepl ref $ adjustSelect (Just (SelectContact xs))
    showPrompt ps
handlers (FindRoom n) = do
  ref <- ask
  ps <- get
  lift $ flip runAff_ (R.findAll n) $ \r -> do
    case r of
      (Left e) -> error $ "Error: " <> message e
      (Right []) ->  error $ "Room<<" <> n <> ">> Not Found."
      (Right [c]) -> runRepl ref $ switchRoom c
      (Right xs) -> do
         error $ "Found Room:\n"
               <> (joinWith "\n" $ mapWithIndex (\i c -> show i <> ". " <> getRoomTopic c) xs)
               <> "\n.select INT 选择聊天群"
         runRepl ref $ adjustSelect (Just (SelectRoom xs))
    showPrompt ps

handlers (Select id0) = do
  ps <- get
  let v0 = do
        idx <- fromString id0
        se <- ps.select
        case se of
          (SelectContact xs) -> do
             v <- xs !! idx
             pure (SelectContact [v])
          (SelectRoom xs) -> do
             v <- xs !! idx
             pure (SelectRoom [v])

  case v0 of
    Just (SelectContact [v]) -> switchContact v
    Just (SelectRoom [v]) -> switchRoom v
    _ -> pure unit

  showPrompt_


handlers (Msg m) = do
  ps <- get
  lift $ flip runAff_ (sendMessage ps.state m) $ \r -> do
    case r of
      (Left e) -> error $ "Error: " <> message e
      (Right _) -> pure unit
    showPrompt ps

handlers (AddWhitelist xs) = replaceWhitelist addWhitelist xs *> showPrompt_
handlers (RemoveWhitelist xs) = replaceWhitelist removeWhitelist xs *> showPrompt_
handlers ClearWhitelist = replaceWhitelist clearWhitelist "" *> showPrompt_
handlers ShowWhitelist = do
  ps <- get
  lift $ error $ "Whitelist:\n" <> joinWith "\n" ps.whitelist

handlers Exit = switchManager *> showPrompt_

handlers Help = do
  lift $ error $ "\n" <> joinWith "\n" help
  showPrompt_

handlers Empty = showPrompt_

sendMessage :: StateType -> String -> Aff Unit
sendMessage (IsContact c) = runContactT c <<< say
sendMessage (IsRoom c) = runRoomT c <<< R.say
sendMessage (IsManager c) = runContactT c <<< say
sendMessage IsEmpty = \_ -> pure unit

launchRepl :: Ref ReplState -> Effect Unit
launchRepl ref = do
  runRepl ref switchManager
  ps <- read ref
  setLineHandler ps.interface (mkLineHandler ref handlers)
  showPrompt ps

showPrompt :: ReplState -> Effect Unit
showPrompt ps = prompt ps.interface

showPrompt_ :: Repl Unit
showPrompt_ = lift <<< showPrompt =<< get

checkWhitelist :: Ref ReplState -> String -> Effect Unit -> Effect Unit
checkWhitelist ref h io = do
  ps <- read ref
  if A.null ps.whitelist then io
    else if elem h ps.whitelist then io else pure unit
