module Robot
  ( subscriberHandler
  , managerHandler
  , roomSubscriberHandler
  ) where

import Prelude

import Control.Monad.Eff.Now (NOW)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff.Class (liftEff)
import DB (DB, Group(..), Message(..), createMessage, deleteMessage, getGroup, getMessage, getMessageList, message, mkGroup, roomSubscribeMessage, saveGroup, setContent, setGroupRepeat, setSchedAt, setUser, subscribeMessage, unRoomSubscribeMessage, unSubscribeMessage, updateMessage)
import Data.Array ((!!), concat)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (trim, drop, length, null, joinWith, dropWhile, takeWhile)
import Data.String.Regex (Regex, match, regex, test)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)
import Periodic.Client (Client, PERIODIC, submitJob, removeJob)
import Utils (startsWith, momentFormat, convertSchedAt, parseTimeString, formatTimeString, getTimeStamp)
import Wechaty.Contact (Contact, ContactT, contactName, say)
import Wechaty.Room (sayTo, roomTopic, RoomT)
import Wechaty.Types (WECHATY)

-- 1-1 3-20 16:10 内容
reCreateMsg :: Regex
reCreateMsg = unsafePartial
  $ fromRight
  $ regex "^(\\d+)-(\\d+)\\s+(\\d+)-(\\d+)\\s+(\\d+):(\\d+)" noFlags

-- 1-1 10s 1h 内容
reCreateDoLaterMsg :: Regex
reCreateDoLaterMsg = unsafePartial
  $ fromRight
  $ regex "^(\\d+)-(\\d+)\\s+([0-9dhms ]+)" noFlags


reShowMsg :: Regex
reShowMsg = unsafePartial
  $ fromRight
  $ regex "^(\\d+)-(\\d+)" noFlags

reNum :: Regex
reNum = unsafePartial $ fromRight $ regex "^\\d+$" noFlags

data Action =
    Msg Message
  | LaterMsg Number Message
  | ShowGroup String
  | Sub String
  | UnSub String
  | Showp String String
  | SaveGroup Group
  | SetGroupRepeat String Number
  | Help
  | NoAction

parseMessage :: String -> Action
parseMessage xs
  | startsWith xs "场景" = parseNumAction ShowGroup $ trim $ drop 2 xs
  | startsWith xs "group" = parseNumAction ShowGroup $ trim $ drop 5 xs
  | startsWith xs "subscribe" = parseNumAction Sub $ trim $ drop 9 xs
  | startsWith xs "订阅" = parseNumAction Sub $ trim $ drop 2 xs
  | startsWith xs "unsubscribe" = parseNumAction UnSub $ trim $ drop 11 xs
  | startsWith xs "取消订阅" = parseNumAction UnSub $ trim $ drop 4 xs
  | startsWith xs "帮助" = Help
  | startsWith xs "help" = Help
  | startsWith xs "编辑场景" = parseSaveGroupAction $ trim $ drop 4 xs
  | startsWith xs "重复场景" = parseSetGroupRepeatAction $ trim $ drop 4 xs
  | test reCreateMsg xs = parseMsgAction xs
  | test reCreateDoLaterMsg xs = parseDoLaterMsgAction xs
  | test reShowMsg xs = parseShowAction xs
  | otherwise = NoAction

parseNumAction ::  (String -> Action) -> String -> Action
parseNumAction f xs
  | test reNum xs = f xs
  | otherwise = NoAction

parseSaveGroupAction :: String -> Action
parseSaveGroupAction xs = go (takeWhile (_ /= ' ') xs) (trim $ dropWhile (_ /= ' ') xs)
  where go :: String -> String -> Action
        go g n | test reNum g || not (null n) = SaveGroup $ mkGroup g n
               | otherwise = NoAction

parseSetGroupRepeatAction :: String -> Action
parseSetGroupRepeatAction xs = go (takeWhile (_ /= ' ') xs) (trim $ dropWhile (_ /= ' ') xs)
  where go :: String -> String -> Action
        go g n | test reNum g = SetGroupRepeat g $ parseTimeString n
               | otherwise = NoAction

parseMsgAction :: String -> Action
parseMsgAction xs = unsafePartial $ fromMaybe NoAction go
  where go :: Maybe Action
        go = do
           m <- match reCreateMsg xs
           group  <- unsafePartial $ fromJust <$> m !! 1
           seq    <- unsafePartial $ fromJust <$> m !! 2
           month  <- unsafePartial $ fromJust <$> m !! 3
           date   <- unsafePartial $ fromJust <$> m !! 4
           hour   <- unsafePartial $ fromJust <$> m !! 5
           minute <- unsafePartial $ fromJust <$> m !! 6
           h      <- unsafePartial $ fromJust <$> m !! 0
           let content = trim $ drop (length h) xs

           pure
            $ Msg
            $ setSchedAt (convertSchedAt
              { month: month
              , date: date
              , hour: hour
              , minute: minute
              })
            $ setContent content
            $ message group seq

parseDoLaterMsgAction :: String -> Action
parseDoLaterMsgAction xs = unsafePartial $ fromMaybe NoAction go
  where go :: Maybe Action
        go = do
           m <- match reCreateDoLaterMsg xs

           group  <- unsafePartial $ fromJust <$> m !! 1
           seq    <- unsafePartial $ fromJust <$> m !! 2
           later  <- unsafePartial $ fromJust <$> m !! 3
           h      <- unsafePartial $ fromJust <$> m !! 0
           let content = trim $ drop (length h) xs

           pure
            $ LaterMsg (parseTimeString later)
            $ setContent content
            $ message group seq


parseShowAction :: String -> Action
parseShowAction xs = unsafePartial $ fromMaybe NoAction go
  where go :: Maybe Action
        go = do
           m <- match reShowMsg xs
           group <- unsafePartial $ fromJust <$> m !! 1
           seq   <- unsafePartial $ fromJust <$> m !! 2
           pure $ Showp group seq

subscriberHandler
  :: forall m eff. MonadAff (db :: DB, wechaty :: WECHATY | eff) m
  => String -> ContactT m Unit
subscriberHandler xs = handleSubscriberAction (parseMessage xs)

managerHandler
  :: forall m eff. MonadAff (db :: DB, wechaty :: WECHATY, periodic :: PERIODIC, now :: NOW | eff) m
  => Client -> String -> ContactT m Unit
managerHandler client xs = handleManagerAction client (parseMessage xs)

roomSubscriberHandler
  :: forall m0 eff. MonadAff (db :: DB, wechaty :: WECHATY, periodic :: PERIODIC | eff) m0
  => Contact -> Boolean -> String -> RoomT m0 Unit
roomSubscriberHandler contact manager xs =
  go $ \m -> handleRoomSubscriberAction contact manager (parseMessage m)

  where go :: forall m. Applicative m => (String -> RoomT m Unit) -> RoomT m Unit
        go f | startsWith xs "@小云" = f $ trim $ drop 3 xs
             | startsWith xs "@机器人" = f $ trim $ drop 4 xs
             | startsWith xs "@robot" = f $ trim $ drop 6 xs
             | startsWith xs "@xiaoyun" = f $ trim $ drop 8 xs
             | otherwise = pure unit

handleManagerAction
  :: forall m eff. MonadAff (db :: DB, wechaty :: WECHATY, periodic :: PERIODIC, now :: NOW | eff) m
  => Client -> Action -> ContactT m Unit
handleManagerAction client (Msg (Message m)) = do
  m0 <- liftAff $ getMessage m.group m.seq
  uid <- contactName
  case m0 of
    Nothing -> do
      liftAff $ createMessage (setUser uid $ Message m)
      liftAff $ submitJob client
        { func: "send-message"
        , name: m.group <> "-" <> m.seq
        , sched_at: m.sched_at
        }
      say $ "场景" <> m.group <> "脚本" <> m.seq <> " 增加成功"
    Just _ -> if null m.content then do
                liftAff $ deleteMessage m.group m.seq
                liftAff $ removeJob client
                  { func: "send-message"
                  , name: m.group <> "-" <> m.seq
                  }
                say $ "场景" <> m.group <> "脚本" <> m.seq <> " 删除成功"
                else do
                  liftAff $ updateMessage (Message m)
                  liftAff $ submitJob client
                    { func: "send-message"
                    , name: m.group <> "-" <> m.seq
                    , sched_at: m.sched_at
                    }
                  say $ "场景" <> m.group <> "脚本" <> m.seq <> " 修改成功"

handleManagerAction client (LaterMsg later m) = do
  now <- liftEff $ getTimeStamp
  handleManagerAction client (Msg $ setSchedAt (now + later) m)

handleManagerAction _ (SaveGroup (Group g)) = do
  n <- contactName
  liftAff $ saveGroup (setUser n $ Group g)
  say $ "场景" <> g.group <> " 修改成功"

handleManagerAction _ (SetGroupRepeat g t) = do
  liftAff $ setGroupRepeat g t
  if t > 0.0 then say $ "修改场景" <> g <> " " <> formatTimeString t <> " 成功"
    else say $ "取消重复场景" <> g <> " 成功"

handleManagerAction _ Help = do
  say $ joinWith "\n" $ concat
    [ [ "场景脚本操作"
      , "输入：1-1 3-19 20:01 详细内容xxxxxx"
      , "输出：场景1脚本1 增加成功"
      , "（《1-1》是场景1的脚本1 ）"
      , ""
      , "再次输入：1-1  3-19 20:01 详细内容xxxxxx"
      , "输出：场景1脚本1 修改成功"
      , "（详细内容为空，输出：场景1脚本1删除成功）"
      , ""
      , "输入：1-1 10s 详细内容xxxxxx"
      , "输出：场景1脚本1 增加成功"
      , "（10s 为 10s 后执行，时间可以 d h m s）"
      , ""
      , "编辑场景"
      , "输入: 编辑场景 1 提醒"
      , "输出: 场景1 修改成功"
      , ""
      , "重复场景"
      , "输入: 重复场景 1 1h"
      , "输出: 修改场景1 成功"
      , "(时间可以是  1d 1h 1m 1s, 如果时间为空则输出: 取消重复场景1)"
      ]
    , showHelp
    , subscriberHelp
    ]

handleManagerAction _ act = handleSubscriberAction act

handleSubscriberAction
  :: forall m eff. MonadAff (db :: DB, wechaty :: WECHATY | eff) m
  => Action -> ContactT m Unit
handleSubscriberAction (ShowGroup group) = do
  mList <- liftAff
    $ map (\(Message m) -> m.group <> "-" <> m.seq)
    <$> getMessageList group

  g <- liftAff $ getGroup group

  let h = case g of
            Nothing -> ""
            Just (Group g') -> "场景: " <> g'.name <> "\n" <> "重复: " <> formatTimeString g'.repeat <> "\n"

  say $ h <> "回复代码查看脚本:\n" <> joinWith "\n" mList

handleSubscriberAction (Showp group seq) = do
  m <- liftAff $ getMessage group seq
  case m of
    Nothing -> say $ "场景" <> group <> "脚本" <> seq <> " 不存在"
    Just (Message m0) -> do
      say $ joinWith "\n"
        [ "场景" <> group <> "脚本" <> seq <> ":"
        , "时间: " <> momentFormat m0.sched_at "YYYY-MM-DD HH:mm:ss"
        , m0.content
        ]

handleSubscriberAction (Sub group) = do
  uid <- contactName
  liftAff $ subscribeMessage uid group
  say $ "订阅场景" <> group <> "成功"

handleSubscriberAction (UnSub group) = do
  uid <- contactName
  liftAff $ unSubscribeMessage uid group
  say $ "取消订阅场景" <> group <> "成功"

handleSubscriberAction Help =
  say $ joinWith "\n" $ concat [showHelp, subscriberHelp]
handleSubscriberAction _ = pure unit

subscriberHelp :: Array String
subscriberHelp =
  [ "订阅场景"
  , "输入: 订阅1"
  , "输出: 订阅场景1 成功"
  , ""
  , "取消订阅场景"
  , "输入: 取消订阅1"
  , "输出: 取消订阅场景1 成功"
  ]

showHelp :: Array String
showHelp =
  [ "查看场景"
  , "输入 ：场景1"
  , "输出：场景所包含的脚本"
  , ""
  , "查看脚本"
  , "输入：1-1"
  , "输出：脚本8详细内容"
  , ""
  , "帮助"
  , "输入：帮助"
  , "输出：帮助内容"
  ]

handleRoomSubscriberAction
  :: forall m eff. MonadAff (db :: DB, wechaty :: WECHATY | eff) m
  => Contact -> Boolean -> Action -> RoomT m Unit
handleRoomSubscriberAction contact _ (ShowGroup group) = do
  mList <- liftAff
    $ map (\(Message m) -> m.group <> "-" <> m.seq)
    <$> getMessageList group

  g <- liftAff $ getGroup group

  let h = case g of
            Nothing -> ""
            Just (Group g') -> "场景: " <> g'.name <> "\n" <> "重复: " <> formatTimeString g'.repeat <> "\n"

  sayTo contact $ "\n" <> h <> "回复代码查看脚本:\n" <> joinWith "\n" mList

handleRoomSubscriberAction contact true (Sub group) = do
  rid <- roomTopic
  liftAff $ roomSubscribeMessage rid group
  sayTo contact $ "订阅场景" <> group <> "成功"
handleRoomSubscriberAction contact true (UnSub group) = do
  rid <- roomTopic
  liftAff $ unRoomSubscribeMessage rid group
  sayTo contact $ "取消订阅场景" <> group <> "成功"
handleRoomSubscriberAction contact _ (Showp group seq) = do
  m <- liftAff $ getMessage group seq
  case m of
    Nothing -> sayTo contact $ "场景" <> group <> "脚本" <> seq <> " 不存在"
    Just (Message m0) -> do
      sayTo contact $ joinWith "\n"
        [ "场景" <> group <> "脚本" <> seq <> ":"
        , "时间: " <> momentFormat m0.sched_at "YYYY-MM-DD HH:mm:ss"
        , m0.content
        ]
handleRoomSubscriberAction contact true Help =
  sayTo contact $ joinWith "\n" $ concat [showHelp, subscriberHelp]
handleRoomSubscriberAction contact false Help =
  sayTo contact $ joinWith "\n" showHelp

handleRoomSubscriberAction contact manager _ = pure unit
