module Robot
  ( subscriberHandler
  , managerHandler
  ) where

import Prelude
import Data.String (trim, drop, length, null, joinWith)
import Data.String.Regex (test, regex, Regex, match)
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafePartial)
import Data.Either (fromRight)
import Data.Array ((!!))
import DB (DB, Message (..), message, setContent, setSchedAt, saveUser, user,
           getMessageList, getMessage, deleteMessage, createMessage, setUserId,
           updateMessage, subscribeMessage, unSubscribeMessage)
import Data.Maybe (Maybe (..), fromMaybe, fromJust)
import Wechaty.Types (ContactM)
import Wechaty.Contact (say, contactId, contactName)
import Control.Monad.Trans.Class (lift)
import Periodic.Client (Client, PERIODIC, submitJob, removeJob)

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
  | Group String
  | Sub String
  | UnSub String
  | Showp String String
  | Help
  | NoAction

parseMessage :: String -> Action
parseMessage xs
  | startsWith xs "场景" = parseNumAction Group $ trim $ drop 2 xs
  | startsWith xs "group" = parseNumAction Group $ trim $ drop 5 xs
  | startsWith xs "subscribe" = parseNumAction Sub $ trim $ drop 9 xs
  | startsWith xs "订阅" = parseNumAction Sub $ trim $ drop 2 xs
  | startsWith xs "unsubscribe" = parseNumAction UnSub $ trim $ drop 11 xs
  | startsWith xs "取消订阅" = parseNumAction UnSub $ trim $ drop 4 xs
  | startsWith xs "帮助" = Help
  | startsWith xs "help" = Help
  | test reCreateMsg xs = parseMsgAction xs
  | test reShowMsg xs = parseShowAction xs
  | otherwise = NoAction

parseNumAction ::  (String -> Action) -> String -> Action
parseNumAction f xs
  | test reNum xs = f xs
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
subscriberHandler xs = do
  uid <- contactId
  name <- contactName
  lift $ saveUser $ user uid name
  handleSubscriberAction (parseMessage xs)

managerHandler :: forall eff. Client -> String -> ContactM (db :: DB, periodic :: PERIODIC | eff) Unit
managerHandler client xs = do
  uid <- contactId
  name <- contactName
  lift $ saveUser $ user uid name
  handleManagerAction client (parseMessage xs)

handleManagerAction :: forall eff. Client -> Action -> ContactM (db :: DB, periodic :: PERIODIC | eff) Unit
handleManagerAction client (Msg (Message m)) = do
  m0 <- lift $ getMessage m.group m.seq
  uid <- contactId
  case m0 of
    Nothing -> do
      lift $ createMessage (setUserId uid $ Message m)
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

handleManagerAction _ Help = do
  say $ joinWith "\n"
    [ "场景脚本操作"
    , "输入：1-1 3-19 20:01 详细内容xxxxxx"
    , "输出：场景1脚本1 增加成功"
    , "（《1-1》是场景1的脚本1 ）"
    , "再次输入：1-1  3-19 20:01 详细内容xxxxxx"
    , "输出：场景1脚本1 修改成功"
    , "（详细内容为空，输出：场景1脚本1删除成功）"
    ]
  saySubscriberHelp

handleManagerAction _ act = handleSubscriberAction act

handleSubscriberAction :: forall eff. Action -> ContactM (db :: DB | eff) Unit
handleSubscriberAction (Group group) = do
  mList <- lift
    $ map (\(Message m) -> m.group <> "-" <> m.seq)
    <$> getMessageList group
  say $ "回复代码查看脚本:\n" <> joinWith "\n" mList

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
  uid <- contactId
  lift $ subscribeMessage uid group
  say $ "订阅场景" <> group <> "成功"

handleSubscriberAction (UnSub group) = do
  uid <- contactId
  lift $ unSubscribeMessage uid group
  say $ "取消订阅场景" <> group <> "成功"

handleSubscriberAction Help = saySubscriberHelp
handleSubscriberAction _ = pure unit

saySubscriberHelp :: forall eff. ContactM (db :: DB | eff) Unit
saySubscriberHelp =
  say $ joinWith "\n"
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
    , ""
    , "订阅场景"
    , "输入: 订阅1"
    , "输出: 订阅场景1 成功"
    , ""
    , "取消订阅场景"
    , "输入: 取消订阅1"
    , "输出: 取消订阅场景1 成功"
    ]
