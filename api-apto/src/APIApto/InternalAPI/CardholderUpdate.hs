module APIApto.InternalAPI.CardholderUpdate where

import qualified APIApto.Cardholder.Update     as CU
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Shared.WebAPI.General.Issuer   ( CreateCardholderAction(..) )

updateCardholder
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CreateCardholderAction
updateCardholder trace userId = do
  CU.updateCardholder trace userId
