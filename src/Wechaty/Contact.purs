module Wechaty.Contact
  ( find
  , say
  , getContactId
  , getContactName
  , contactId
  , contactName
  ) where

import Prelude
import Control.Promise (Promise, toAff)
import Wechaty.Types (Contact, ContactM)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Function.Uncurried (Fn2, runFn2)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe (..))

foreign import _find :: forall eff. String
                     -> (Contact -> Maybe Contact)
                     -> Maybe Contact
                     -> Eff eff (Promise (Maybe Contact))

find :: forall eff. String -> Aff eff (Maybe Contact)
find n = liftEff (_find n Just Nothing) >>= toAff

foreign import _say :: forall a eff. Fn2 Contact a (Eff eff (Promise Unit))

runSay :: forall a eff. Contact -> a -> Aff eff Unit
runSay contact a = liftEff (runFn2 _say contact a) >>= toAff

say :: forall a eff. a -> ContactM eff Unit
say a = do
  contact <- ask
  lift $ runSay contact a

contactId :: forall eff. ContactM eff String
contactId = getContactId <$> ask

contactName :: forall eff. ContactM eff String
contactName = getContactName <$> ask

foreign import getContactId :: Contact -> String
foreign import getContactName :: Contact -> String
