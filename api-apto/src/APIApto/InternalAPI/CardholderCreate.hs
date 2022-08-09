module APIApto.InternalAPI.CardholderCreate where

import qualified APIApto.Cardholder.Create     as CC
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Shared.WebAPI.General.Issuer   ( CreateCardholderAction(..) )

createCardholder
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CreateCardholderAction
createCardholder trace userId = do
  CC.createCardholder trace userId
