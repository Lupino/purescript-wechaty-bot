module Wechaty.Contact
  ( Contact
  , ContactT
  , runContactT
  , find
  , say
  , getContactName
  , contactName
  , self
  , findAll
  , module Wechaty.Types
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Wechaty.Types (WECHATY)

foreign import data Contact :: Type
type ContactT m = ReaderT Contact m

runContactT :: forall a m. Contact -> ContactT m a -> m a
runContactT contact = flip runReaderT contact

foreign import _find :: forall eff. String
                     -> (Contact -> Maybe Contact)
                     -> Maybe Contact
                     -> Eff eff (Promise (Maybe Contact))

find :: forall eff. String -> Aff eff (Maybe Contact)
find n = liftEff (_find n Just Nothing) >>= toAff

foreign import _findAll :: forall eff. String -> Eff eff (Promise (Array Contact))

findAll :: forall eff. String -> Aff eff (Array Contact)
findAll n = liftEff (_findAll n) >>= toAff


foreign import _say :: forall a eff. Fn2 Contact a (Eff eff (Promise Unit))

runSay :: forall a eff. Contact -> a -> Aff eff Unit
runSay contact a = liftEff (runFn2 _say contact a) >>= toAff

say
  :: forall a m eff. MonadAff (wechaty :: WECHATY | eff) m
  => a -> ContactT m Unit
say a = do
  contact <- ask
  liftAff $ runSay contact a

contactName
  :: forall m. Monad m
  => ContactT m String
contactName = getContactName <$> ask

foreign import getContactName :: Contact -> String

foreign import self :: forall eff. Eff (wechaty :: WECHATY | eff) Contact
