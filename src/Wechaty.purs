module Wechaty
  ( initWechaty
  , onLogin
  , onLogout
  , onMessage
  , start
  ) where

import Prelude
import Wechaty.Types (Contact, Wechaty, WechatyM, Message, MessageM, runMessageM, runContactM, ContactM, WECHATY)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (runAff_)
import Control.Monad.Reader (ask)
import Data.Function.Uncurried (Fn2, runFn2)
import Control.Promise (Promise, toAff)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either)
import Control.Monad.Eff.Exception (Error)

foreign import initWechaty :: forall eff. Eff eff Wechaty


doError :: forall eff a. Either Error a → Eff eff Unit
doError _ = pure unit

foreign import _onLogout :: forall eff. Fn2 Wechaty (Contact -> Eff eff Unit) (Eff eff Unit)

onLogout :: forall eff. ContactM eff Unit -> WechatyM eff Unit
onLogout m = do
  bot <- ask
  liftEff $ runFn2 _onLogout bot $ doContact m

foreign import _onLogin :: forall eff. Fn2 Wechaty (Contact -> Eff eff Unit) (Eff eff Unit)

doContact :: forall eff. ContactM eff Unit -> Contact -> Eff (wechaty :: WECHATY | eff) Unit
doContact m contact = runAff_ doError $ runContactM contact m

onLogin :: forall eff. ContactM eff Unit -> WechatyM eff Unit
onLogin m = do
  bot <- ask
  liftEff $ runFn2 _onLogin bot $ doContact m

foreign import _onMessage :: forall eff. Fn2 Wechaty (Message -> Eff eff Unit) (Eff eff Unit)

doMessage :: forall eff. MessageM eff Unit -> Message -> Eff (wechaty :: WECHATY | eff) Unit
doMessage m msg = runAff_ doError $ runMessageM msg m

onMessage :: forall eff. MessageM eff Unit -> WechatyM eff Unit
onMessage m = do
  bot <- ask
  liftEff $ runFn2 _onMessage bot $ doMessage m

foreign import _start :: forall eff. Wechaty -> Eff eff (Promise Unit)

start :: forall eff. WechatyM eff Unit
start = do
  bot <- ask
  lift $ liftEff (_start bot) >>= toAff
