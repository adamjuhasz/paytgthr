module APIApto.Cardholder.Update where

import           APIApto.Cardholder.Create      ( createCardholder )
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )
import           Shared.WebAPI.General.Issuer   ( CreateCardholderAction )

updateCardholder
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CreateCardholderAction
updateCardholder trace cccUser = do
  liftIO $ putStr "updateCardholder started " >> print (cccUser, trace)
  createCardholder trace cccUser
