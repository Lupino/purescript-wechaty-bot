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
import DB (Message (..), messageMod)
import Data.Array (elem, (:), delete, filter, mapWithIndex, (!!))
import Data.Array (null) as A
import Data.Dayjs (now, toUnixTime)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim, drop, length, null, joinWith, takeWhile, dropWhile, codePointFromChar)
import Database.Sequelize (findOne, findAll, create, destory, update) as DB
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Effect.Ref (Ref, new, read, modify)
import Node.ReadLine (Interface, createConsoleInterface, setPrompt, setLineHandler, prompt, Completer)
import Periodic.Client (Client, submitJob)
import Utils (formatDate, adjustTime, startsWith)
import Wechaty.Contact (Contact, name, runContactT, say) as C
import Wechaty.Room (Room, topic, runRoomT, say) as R
import Wechaty (runWechatyT, Wechaty, findContactAll, findRoomAll, userSelf)
import Data.Traversable (for)

type Whitelist = Array String

data StateType = IsContact C.Contact
               | IsRoom R.Room
               | IsManager C.Contact
               | IsEmpty

data Select = SelectRoom (Array R.Room)
            | SelectContact (Array C.Contact)

type ReplState =
  { whitelist :: Whitelist
  , interface :: Interface
  , select :: Maybe Select
  , state :: StateType
  , periodic :: Client
  , wechaty :: Wechaty
  }

type Repl a = ReaderT (Ref ReplState) Aff a

runRepl :: forall a. Ref ReplState -> Repl a -> Aff a
runRepl s m = runReaderT m s

initReplState :: Wechaty -> Client -> Effect (Ref ReplState)
initReplState w c = do
  rl <- createConsoleInterface completion
  new {whitelist: [], interface: rl, select: Nothing, state: IsEmpty, periodic: c, wechaty: w}

get :: Repl ReplState
get = do
  ref <- ask
  liftEffect $ read ref

setContactPrompt :: C.Contact -> Interface -> Aff Unit
setContactPrompt c rl = do
  n <- C.runContactT c C.name
  let ps = "Contact<<" <> n <> ">> "
  liftEffect $ setPrompt ps (length ps) rl

setRoomPrompt :: R.Room -> Interface -> Aff Unit
setRoomPrompt r rl = do
  t <- R.runRoomT r R.topic
  let ps = "Room<<" <> t <> ">> "
  liftEffect $ setPrompt ps (length ps) rl

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
  void $ liftEffect $ modify (\ps -> ps {whitelist = f xs ps.whitelist}) ref

adjustSelect :: Maybe Select -> Repl Unit
adjustSelect xs = do
  ref <- ask
  void $ liftEffect $ modify (\ps -> ps {select = xs}) ref

switch :: forall a. (a -> Interface -> Aff Unit) -> (a -> StateType) -> a -> Repl Unit
switch p f a = do
  ref <- ask
  ps <- liftEffect $ modify (\rs -> rs {state = f a}) ref
  lift $ p a ps.interface

switchContact :: C.Contact -> Repl Unit
switchContact = switch setContactPrompt IsContact

switchManager :: Repl Unit
switchManager = do
  ref <- ask
  ps <- get
  c <- lift $ runWechatyT launchAff_ ps.wechaty userSelf
  switch setContactPrompt IsManager c

switchRoom :: R.Room -> Repl Unit
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
mkLineHandler ps f = launchAff_ <<< runRepl ps <<< f <<< parseCmd

handlers :: Cmd -> Repl Unit
handlers (FindContact n) = do
  ps <- get
  contacts <- lift $ runWechatyT launchAff_ ps.wechaty $ findContactAll n
  case contacts of
    [] -> liftEffect $ error $ "Contact<<" <> n <> ">> Not Found."
    [c] -> switchContact c
    xs -> do
      ns <- lift $ for xs $ \x -> C.runContactT x C.name
      liftEffect
        $ error
        $ "Found Contact:\n"
        <> (joinWith "\n" $ mapWithIndex (\i c -> show i <> ". " <> c) ns)
        <> "\n.select INT 选择用户"
      adjustSelect (Just (SelectContact xs))
      showPrompt
handlers (FindRoom n) = do
  ps <- get
  rooms <- lift $ runWechatyT launchAff_ ps.wechaty $ findRoomAll n
  case rooms of
    [] -> liftEffect $ error $ "Room<<" <> n <> ">> Not Found."
    [r] -> switchRoom r
    xs -> do
      ns <- lift $ for xs $ \x -> R.runRoomT x R.topic
      liftEffect
        $ error
        $ "Found Room:\n"
        <> (joinWith "\n" $ mapWithIndex (\i c -> show i <> ". " <> c) ns)
        <> "\n.select INT 选择聊天群"
      adjustSelect (Just (SelectRoom xs))
      showPrompt

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

  showPrompt

handlers (Msg m) = do
  ps <- get
  lift $ sendMessage ps.state m
  showPrompt

handlers (AddWhitelist xs) = replaceWhitelist addWhitelist xs *> showPrompt
handlers (RemoveWhitelist xs) = replaceWhitelist removeWhitelist xs *> showPrompt
handlers ClearWhitelist = replaceWhitelist clearWhitelist "" *> showPrompt
handlers ShowWhitelist = do
  ps <- get
  liftEffect $ error $ "Whitelist:\n" <> joinWith "\n" ps.whitelist
  showPrompt

handlers (AddTask msg) = do
  ps <- get
  lift $ go ps.state
  showPrompt

  where go :: StateType -> Aff Unit
        go (IsContact c) = saveContactTask c
        go (IsRoom r) = saveRoomTask r
        go (IsManager c) = saveContactTask c
        go IsEmpty = pure unit

        saveContactTask :: C.Contact -> Aff Unit
        saveContactTask c = do
          n <- C.runContactT c C.name
          t <- DB.create messageMod {user: "user-" <> n, message: msg}
          liftEffect $ showHelp t

        saveRoomTask :: R.Room -> Aff Unit
        saveRoomTask r = do
          rt <- R.runRoomT r R.topic
          t <- DB.create messageMod {user: "room-" <> rt, message: msg}
          liftEffect $ showHelp t

        showHelp :: Message -> Effect Unit
        showHelp (Message t) =
          error $ "Task[" <>  show t.id <> "] created.\n"
            <> "Use task schedin " <> show t.id
            <> " STRING  to set the sched time"

handlers ListTask = do
  r0 <- lift $ DB.findAll messageMod {}
  liftEffect $ error $ joinWith "\n" $ map formatTask r0
  showPrompt

handlers (GetTask id) = do
  r <- lift $ DB.findOne messageMod {where: {id: id}}
  case r of
    Nothing -> liftEffect $ error $ "Not Found."
    Just t -> liftEffect $ error $ formatTask t
  showPrompt

handlers (SetTaskSchedIn id later) = do
  ps <- get
  lift $ go ps
  showPrompt

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
  lift $ DB.update messageMod {repeat: repeat} {where: {id: id}}
  liftEffect $ error "Repeat changed."
  showPrompt

handlers (DelTask id) = do
  ps <- get
  lift $ DB.destory messageMod {where: {id: id}}
  liftEffect $ error $ "Task " <> show id <> " deleted."
  showPrompt

handlers Exit = switchManager *> showPrompt

handlers Help = do
  liftEffect $ error $ "\n" <> joinWith "\n" help
  showPrompt

handlers Empty = showPrompt

formatTask :: Message -> String
formatTask (Message t) = joinWith "\n"
  [ "id: " <> show t.id
  , "user: " <> t.user
  , "message: " <> t.message
  , "sched_at: " <> formatDate t.sched_at
  , "repeat: " <> t.repeat
  ]

sendMessage :: StateType -> String -> Aff Unit
sendMessage (IsContact c) = C.runContactT c <<< C.say
sendMessage (IsRoom c) = R.runRoomT c <<< R.say
sendMessage (IsManager c) = C.runContactT c <<< C.say
sendMessage IsEmpty = \_ -> pure unit

launchRepl :: Ref ReplState -> Aff Unit
launchRepl ref = do
  runRepl ref switchManager
  ps <- liftEffect $ read ref
  liftEffect $ setLineHandler ps.interface (mkLineHandler ref handlers)
  runRepl ref showPrompt

showPrompt :: Repl Unit
showPrompt = do
  ps <- get
  liftEffect $ prompt ps.interface

checkWhitelist :: Ref ReplState -> String -> Effect Unit -> Effect Unit
checkWhitelist ref h io = do
  ps <- read ref
  if A.null ps.whitelist then io
    else if elem h ps.whitelist then io else pure unit
