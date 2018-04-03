module Robot
  ( subscriberHandler
  , managerHandler
  , roomSubscriberHandler
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import DB (DB, Message(..), message, setContent, setSchedAt, getMessageList,
           getMessage, deleteMessage, createMessage, setUser, updateMessage,
           subscribeMessage, unSubscribeMessage, roomSubscribeMessage,
           unRoomSubscribeMessage, Group(..), mkGroup, saveGroup, getGroup,
           setGroupRepeat, parseTimeString, formatTimeString)
import Data.Array ((!!), concat)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.String (trim, drop, length, null, joinWith, dropWhile, takeWhile)
import Data.String.Regex (Regex, match, regex, test)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)
import Periodic.Client (Client, PERIODIC, submitJob, removeJob)
import Wechaty.Contact (contactName, say)
import Wechaty.Room (sayTo, roomTopic)
import Wechaty.Types (Contact, ContactM, RoomM)

foreign import startsWith :: String -> String -> Boolean
foreign import convertSchedAt :: forall a. a -> Number
foreign import momentFormat :: Number -> String -> String

reCreateMsg :: Regex
reCreateMsg = unsafePartial
  $ fromRight
  $ regex "^(\\d+)-(\\d+)\\s*(\\d+)-(\\d+)\\s*(\\d+):(\\d+)" noFlags

reShowMsg :: Regex
reShowMsg = unsafePartial
  $ fromRight
  $ regex "^(\\d+)-(\\d+)" noFlags

reNum :: Regex
reNum = unsafePartial $ fromRight $ regex "^\\d+$" noFlags

data Action =
    Msg Message
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

parseShowAction :: String -> Action
parseShowAction xs = unsafePartial $ fromMaybe NoAction go
  where go :: Maybe Action
        go = do
           m <- match reShowMsg xs
           group <- unsafePartial $ fromJust <$> m !! 1
           seq   <- unsafePartial $ fromJust <$> m !! 2
           pure $ Showp group seq

subscriberHandler :: forall eff. String -> ContactM (db :: DB | eff) Unit
subscriberHandler xs = handleSubscriberAction (parseMessage xs)

managerHandler :: forall eff. Client -> String -> ContactM (db :: DB, periodic :: PERIODIC | eff) Unit
managerHandler client xs = handleManagerAction client (parseMessage xs)

roomSubscriberHandler :: forall eff. Contact -> Boolean -> String -> RoomM (db :: DB | eff) Unit
roomSubscriberHandler contact manager xs =
  go $ \m -> handleRoomSubscriberAction contact manager (parseMessage m)

  where go :: forall e. (String -> RoomM (db :: DB | e) Unit) -> RoomM (db :: DB | e) Unit
        go f | startsWith xs "@小云" = f $ trim $ drop 3 xs
             | startsWith xs "@机器人" = f $ trim $ drop 4 xs
             | startsWith xs "@robot" = f $ trim $ drop 6 xs
             | startsWith xs "@xiaoyun" = f $ trim $ drop 8 xs
             | otherwise = pure unit

handleManagerAction :: forall eff. Client -> Action -> ContactM (db :: DB, periodic :: PERIODIC | eff) Unit
handleManagerAction client (Msg (Message m)) = do
  m0 <- lift $ getMessage m.group m.seq
  uid <- contactName
  case m0 of
    Nothing -> do
      lift $ createMessage (setUser uid $ Message m)
      lift $ submitJob client
        { func: "send-message"
        , name: m.group <> "-" <> m.seq
        , sched_at: m.sched_at
        }
      say $ "场景" <> m.group <> "脚本" <> m.seq <> " 增加成功"
    Just _ -> if null m.content then do
                lift $ deleteMessage m.group m.seq
                lift $ removeJob client
                  { func: "send-message"
                  , name: m.group <> "-" <> m.seq
                  }
                say $ "场景" <> m.group <> "脚本" <> m.seq <> " 删除成功"
                else do
                  lift $ updateMessage (Message m)
                  lift $ submitJob client
                    { func: "send-message"
                    , name: m.group <> "-" <> m.seq
                    , sched_at: m.sched_at
                    }
                  say $ "场景" <> m.group <> "脚本" <> m.seq <> " 修改成功"

handleManagerAction _ (SaveGroup (Group g)) = do
  n <- contactName
  lift $ saveGroup (setUser n $ Group g)
  say $ "场景" <> g.group <> " 修改成功"

handleManagerAction _ (SetGroupRepeat g t) = do
  lift $ setGroupRepeat g t
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

handleSubscriberAction :: forall eff. Action -> ContactM (db :: DB | eff) Unit
handleSubscriberAction (ShowGroup group) = do
  mList <- lift
    $ map (\(Message m) -> m.group <> "-" <> m.seq)
    <$> getMessageList group

  g <- lift $ getGroup group

  let h = case g of
            Nothing -> ""
            Just (Group g') -> "场景: " <> g'.name <> "\n" <> "重复: " <> formatTimeString g'.repeat <> "\n"

  say $ h <> "回复代码查看脚本:\n" <> joinWith "\n" mList

handleSubscriberAction (Showp group seq) = do
  m <- lift $ getMessage group seq
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
  lift $ subscribeMessage uid group
  say $ "订阅场景" <> group <> "成功"

handleSubscriberAction (UnSub group) = do
  uid <- contactName
  lift $ unSubscribeMessage uid group
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

handleRoomSubscriberAction :: forall eff. Contact -> Boolean -> Action -> RoomM (db :: DB | eff) Unit
handleRoomSubscriberAction contact _ (ShowGroup group) = do
  mList <- lift
    $ map (\(Message m) -> m.group <> "-" <> m.seq)
    <$> getMessageList group

  g <- lift $ getGroup group

  let h = case g of
            Nothing -> ""
            Just (Group g') -> "场景: " <> g'.name <> "\n" <> "重复: " <> formatTimeString g'.repeat <> "\n"

  sayTo contact $ "\n" <> h <> "回复代码查看脚本:\n" <> joinWith "\n" mList

handleRoomSubscriberAction contact true (Sub group) = do
  rid <- roomTopic
  lift $ roomSubscribeMessage rid group
  sayTo contact $ "订阅场景" <> group <> "成功"
handleRoomSubscriberAction contact true (UnSub group) = do
  rid <- roomTopic
  lift $ unRoomSubscribeMessage rid group
  sayTo contact $ "取消订阅场景" <> group <> "成功"
handleRoomSubscriberAction contact _ (Showp group seq) = do
  m <- lift $ getMessage group seq
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
