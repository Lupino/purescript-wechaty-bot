module Wechaty
  ( Wechaty
  , WechatyConfig
  , WechatyT
  , runWechatyT
  , initWechaty
  , onScan
  , showQrcode
  , onError
  , onLogin
  , onLogout
  , onMessage
  , start
  , module Wechaty.Types
  ) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, runFn2)
import Wechaty.Contact (Contact, ContactT, runContactT)
import Wechaty.Message (MessageT, Message, runMessageT)
import Wechaty.Types (WECHATY)

foreign import data Wechaty :: Type

data WechatyConfig eff m = WechatyConfig (m Unit -> Eff (wechaty :: WECHATY | eff) Unit) Wechaty

type WechatyT eff m = ReaderT (WechatyConfig eff m) m

runWechatyT
  :: forall a m eff. (m Unit -> Eff (wechaty :: WECHATY | eff) Unit)
  -> Wechaty -> WechatyT eff m a -> m a
runWechatyT runEff wechaty = flip runReaderT (WechatyConfig runEff wechaty)

foreign import initWechaty :: forall eff. Eff eff Wechaty

foreign import _onScan :: forall eff. Wechaty -> (String -> Int -> Eff eff Unit) -> (Eff eff Unit)

onScan
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => (String -> Int -> Eff (wechaty :: WECHATY | eff) Unit) -> WechatyT eff m Unit
onScan f = do
  (WechatyConfig _ bot) <- ask
  liftEff $ _onScan bot f

foreign import showQrcode :: forall eff. String -> (Eff eff Unit)

foreign import _onError :: forall eff. Wechaty -> (String -> Eff eff Unit) -> (Eff eff Unit)

onError
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => (String -> Eff (wechaty :: WECHATY | eff) Unit) -> WechatyT eff m Unit
onError f = do
  (WechatyConfig _ bot) <- ask
  liftEff $ _onError bot f

foreign import _onLogout :: forall eff. Fn2 Wechaty (Contact -> Eff eff Unit) (Eff eff Unit)

onLogout
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => ContactT m Unit -> WechatyT eff m Unit
onLogout m = do
  (WechatyConfig runEff bot) <- ask
  liftEff $ runFn2 _onLogout bot $ doContact runEff m

foreign import _onLogin :: forall eff. Fn2 Wechaty (Contact -> Eff eff Unit) (Eff eff Unit)

doContact
  :: forall m eff. (m Unit -> Eff (wechaty :: WECHATY | eff) Unit)
  -> ContactT m Unit -> Contact -> Eff (wechaty :: WECHATY | eff) Unit
doContact runEff m contact = runEff $ runContactT contact m

onLogin
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => ContactT m Unit -> WechatyT eff m Unit
onLogin m = do
  (WechatyConfig runEff bot) <- ask
  liftEff $ runFn2 _onLogin bot $ doContact runEff m

foreign import _onMessage :: forall eff. Fn2 Wechaty (Message -> Eff eff Unit) (Eff eff Unit)

doMessage
  :: forall m eff. (m Unit -> Eff (wechaty :: WECHATY | eff) Unit)
   -> MessageT m Unit -> Message -> Eff (wechaty :: WECHATY | eff) Unit
doMessage runEff m = runEff <<< flip runMessageT m

onMessage
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MessageT m Unit -> WechatyT eff m Unit
onMessage m = do
  (WechatyConfig runEff bot) <- ask
  liftEff $ runFn2 _onMessage bot $ doMessage runEff m

foreign import _start :: forall eff. Wechaty -> Eff eff (Promise Unit)

start
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MonadAff (wechaty :: WECHATY | eff) m
  => WechatyT eff m Unit
start = do
  (WechatyConfig _ bot) <- ask
  liftAff $ liftEff (_start bot) >>= toAff
