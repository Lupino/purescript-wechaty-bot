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

import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import DB (MessageType, messageMod)
import Data.Array (elem, (:), delete, filter, mapWithIndex, (!!))
import Data.Array (null) as A
import Data.Dayjs (now, toUnixTime)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim, drop, length, null, joinWith, takeWhile, dropWhile, codePointFromChar)
import Database.Sequelize (findOne, findAll, create, destory, update) as DB
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Effect.Exception (message)
import Effect.Ref (Ref, new, read, modify)
import Node.ReadLine (Interface, createConsoleInterface, setPrompt, setLineHandler, prompt, Completer)
import Periodic.Client (Client, submitJob)
import Utils (formatDate, adjustTime, startsWith)
import Wechaty.Contact (Contact, findAll, getContactName, runContactT, say, self)
import Wechaty.Room (Room, getRoomTopic, runRoomT)
import Wechaty.Room (findAll, say) as R

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
  , periodic :: Client
  }

type Repl a = ReaderT (Ref ReplState) Effect a

runRepl :: forall a. Ref ReplState -> Repl a -> Effect a
runRepl s m = runReaderT m s

initReplState :: Client -> Effect (Ref ReplState)
initReplState c = do
  rl <- createConsoleInterface completion
  new {whitelist: [], interface: rl, select: Nothing, state: IsEmpty, periodic: c}

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
  | AddTask String
  | ListTask
  | GetTask Int
  | SetTaskSchedIn Int String
  | SetTaskRepeat Int String
  | DelTask Int
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
  | startsWith xs ".task add" = AddTask $ trim $ drop 9 xs
  | startsWith xs ".task list" = ListTask
  | startsWith xs ".task get" = fromMaybe Empty $ map GetTask $ fromString $ trim $ drop 9 xs
  | startsWith xs ".task schedin" = parseTaskCmd SetTaskSchedIn $ trim $ drop 13 xs
  | startsWith xs ".task repeat" = parseTaskCmd SetTaskRepeat $ trim $ drop 12 xs
  | startsWith xs ".task del" = fromMaybe Empty $ map DelTask $ fromString $ trim $ drop 9 xs
  | startsWith xs ".exit" = Exit
  | startsWith xs ".help" = Help
  | null xs = Empty
  | otherwise = Msg xs

parseTaskCmd :: (Int -> String -> Cmd) -> String -> Cmd
parseTaskCmd f xs =
  case fromString h of
    Nothing -> Empty
    Just h' -> f h' (trim t)
  where h = takeWhile isSpace xs
        t = dropWhile isSpace xs
        isSpace v = v /= codePointFromChar ' '

hits :: Array String
hits =
  [ ".contact"
  , ".room"
  , ".select"
  , ".whitelist"
  , ".whitelist add"
  , ".whitelist remove"
  , ".whitelist clear"
  , ".task add"
  , ".task list"
  , ".task get"
  , ".task schedin"
  , ".task repeat"
  , ".task del"
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
  , ".task add STRING          -- 添加任务"
  , ".task list                -- 查看所有任务"
  , ".task get INT             -- 查看单条任务"
  , ".task schedin INT STRING  -- 任务多长时间后执行"
  , ".task repeat INT STRING   -- 任务重复周期"
  , ".task del INT             -- 删除任务"
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
  showPrompt_

handlers (AddTask msg) = do
  ps <- get
  lift $ flip runAff_ (go ps.state) $ \r -> do
    case r of
      (Left e) -> error $ "Error: " <> message e
      (Right _) -> pure unit
    showPrompt ps

  where go :: StateType -> Aff Unit
        go (IsContact c) = saveContactTask c
        go (IsRoom r) = saveRoomTask r
        go (IsManager c) = saveContactTask c
        go IsEmpty = pure unit

        saveContactTask :: Contact -> Aff Unit
        saveContactTask c = do
          t <- DB.create messageMod {user: "user-" <> getContactName c, message: msg}
          liftEffect $ showHelp t

        saveRoomTask :: Room -> Aff Unit
        saveRoomTask r = do
          t <- DB.create messageMod {user: "room-" <> getRoomTopic r, message: msg}
          liftEffect $ showHelp t

        showHelp :: forall task. {id :: Int | task} -> Effect Unit
        showHelp t =
          error $ "Task[" <>  show t.id <> "] created.\n"
            <> "Use task schedin " <> show t.id
            <> " STRING  to set the sched time"

handlers ListTask = do
  ps <- get
  lift $ flip runAff_ (DB.findAll messageMod {}) $ \r -> do
    case r of
      Left e -> error $ message e
      Right r0 -> error $ joinWith "\n" $ map formatTask r0

    showPrompt ps

handlers (GetTask id) = do
  ps <- get
  lift $ flip runAff_ (DB.findOne messageMod {where: {id: id}}) $ \r -> do
    case r of
      Left e -> error $ message e
      Right Nothing -> error $ "Not found."
      Right (Just t) -> error $ formatTask t
    showPrompt ps

handlers (SetTaskSchedIn id later) = do
  ps <- get
  lift $ flip runAff_ (go ps) $ \r -> do
    case r of
      Left e -> error $ message e
      Right _ -> pure unit

    showPrompt ps

  where go :: ReplState -> Aff Unit
        go ps = do
          t <- DB.findOne messageMod {where: {id: id}}
          case t of
            Nothing -> liftEffect $ error $ "Task " <> show id <> " is not found."
            Just _ -> do
              schedat <- liftEffect $ toUnixTime <<< adjustTime later <$> now
              DB.update messageMod {sched_at: schedat} {where: {id: id}}
              submitJob ps.periodic {name: show id, func: "send-message", sched_at: schedat}
              liftEffect $ error $ "Modify sched time done."

handlers (SetTaskRepeat id repeat) = do
  ps <- get
  lift $ flip runAff_ (DB.update messageMod {repeat: repeat} {where: {id: id}}) $ \r -> do
    case r of
      Left e -> error $ message e
      Right _ -> error "Repeat changed."

    showPrompt ps

handlers (DelTask id) = do
  ps <- get
  lift $ flip runAff_ (DB.destory messageMod {where: {id: id}}) $ \r -> do
    case r of
      Left e -> error $ message e
      Right _ -> error $ "Task " <> show id <> " deleted."
    showPrompt ps

handlers Exit = switchManager *> showPrompt_

handlers Help = do
  lift $ error $ "\n" <> joinWith "\n" help
  showPrompt_

handlers Empty = showPrompt_

formatTask :: MessageType -> String
formatTask t = joinWith "\n"
  [ "id: " <> show t.id
  , "user: " <> t.user
  , "message: " <> t.message
  , "sched_at: " <> formatDate t.sched_at
  , "repeat: " <> t.repeat
  ]

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
