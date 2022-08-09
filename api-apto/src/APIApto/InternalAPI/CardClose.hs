module APIApto.InternalAPI.CardClose where

import qualified APIApto.Card.Close            as CC
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant                        ( NoContent(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )

closeCard
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m NoContent
closeCard trace userId = do
  CC.closeCard trace userId
  return NoContent
