module Wechaty
  ( Wechaty
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
  , module Exports
  ) where

import Prelude

import Control.Monad.Aff (Aff, launchAff_)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAff)
import Data.Either (Either)
import Data.Function.Uncurried (Fn2, runFn2)
import Wechaty.Types (Contact, ContactM, Message, MessageM, WECHATY, runContactM, runMessageM)
import Wechaty.Types (WECHATY) as Exports

foreign import data Wechaty :: Type
type WechatyT m = ReaderT Wechaty m

runWechatyT :: forall a m. Wechaty -> WechatyT m a -> m a
runWechatyT wechaty = flip runReaderT wechaty

foreign import initWechaty :: forall eff. Eff eff Wechaty

foreign import _onScan :: forall eff. Wechaty -> (String -> Int -> Eff eff Unit) -> (Eff eff Unit)

onScan
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => (String -> Int -> Eff (wechaty :: WECHATY | eff) Unit) -> WechatyT m Unit
onScan f = do
  bot <- ask
  liftEff $ _onScan bot f

foreign import showQrcode :: forall eff. String -> (Eff eff Unit)

foreign import _onError :: forall eff. Wechaty -> (String -> Eff eff Unit) -> (Eff eff Unit)

onError
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => (String -> Eff (wechaty :: WECHATY | eff) Unit) -> WechatyT m Unit
onError f = do
  bot <- ask
  liftEff $ _onError bot f

foreign import _onLogout :: forall eff. Fn2 Wechaty (Contact -> Eff eff Unit) (Eff eff Unit)

onLogout
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => ContactM eff Unit -> WechatyT m Unit
onLogout m = do
  bot <- ask
  liftEff $ runFn2 _onLogout bot $ doContact m

foreign import _onLogin :: forall eff. Fn2 Wechaty (Contact -> Eff eff Unit) (Eff eff Unit)

doContact :: forall eff. ContactM eff Unit -> Contact -> Eff (wechaty :: WECHATY | eff) Unit
doContact m contact = launchAff_ $ runContactM contact m

onLogin
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => ContactM eff Unit -> WechatyT m Unit
onLogin m = do
  bot <- ask
  liftEff $ runFn2 _onLogin bot $ doContact m

foreign import _onMessage :: forall eff. Fn2 Wechaty (Message -> Eff eff Unit) (Eff eff Unit)

doMessage :: forall eff. MessageM eff Unit -> Message -> Eff (wechaty :: WECHATY | eff) Unit
doMessage m msg = launchAff_ $ runMessageM msg m

onMessage
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MessageM eff Unit -> WechatyT m Unit
onMessage m = do
  bot <- ask
  liftEff $ runFn2 _onMessage bot $ doMessage m

foreign import _start :: forall eff. Wechaty -> Eff eff (Promise Unit)

start
  :: forall m eff. MonadEff (wechaty :: WECHATY | eff) m
  => MonadAff (wechaty :: WECHATY| eff) m
  => WechatyT m Unit
start = do
  bot <- ask
  liftAff $ liftEff (_start bot) >>= toAff
