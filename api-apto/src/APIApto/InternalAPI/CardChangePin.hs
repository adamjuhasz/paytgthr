module APIApto.InternalAPI.CardChangePin where

import qualified APIApto.Card.ChangePin        as CP
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Models.Card             ( IssuerPlatform(AptoPayments) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Shared.WebAPI.General.Issuer   ( CardPinChangeBody(..) )

chanegeCardPin
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> CardPinChangeBody
  -> m IssuerPlatform
chanegeCardPin trace userId CardPinChangeBody { encPin = pin } = do
  aptoId <- CP.changeCardPin trace userId pin
  return $ AptoPayments aptoId
