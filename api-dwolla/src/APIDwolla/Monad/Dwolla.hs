module APIDwolla.Monad.Dwolla where

import           APIDwolla.Dwolla.Client        ( ClientErrors )
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString.Lazy          as BL
import           Data.Text                      ( Text )
import           Network.HTTP.Client            ( Response )
import           Shared.Models.Payment          ( Payment )
import           Shared.Models.User             ( UserModel )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasDwollaClient m where
  createCustomer    :: TraceContext -> UserModel -> m (Either ClientErrors ByteString)
  addBankAccount    :: TraceContext -> UserModel -> m (Either ClientErrors Text)
  removeBankAccount :: TraceContext -> Text -> m (Either ClientErrors (Response BL.ByteString))
  createACHTransfer :: TraceContext -> Payment -> UserModel -> m (Either ClientErrors ByteString)
  cancelACHTransfer :: TraceContext -> Payment -> m (Either ClientErrors ())
