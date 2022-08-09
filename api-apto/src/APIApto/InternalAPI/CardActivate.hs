module APIApto.InternalAPI.CardActivate where

import qualified APIApto.Card.Activate         as CA
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant                        ( NoContent(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Shared.WebAPI.General.Issuer   ( CardActivateBody(..) )

activateCard
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> CardActivateBody
  -> m NoContent
activateCard trace userId CardActivateBody { cardLastFour = lastFour } = do
  _ <- CA.activateCard trace userId lastFour
  return NoContent
