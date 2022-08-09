module APIApto.InternalAPI.CardGetLastFour where

import qualified APIApto.Card.Query            as CQ
import           APIApto.Monad.Accounts         ( HasAccounts )
import           APIApto.Monad.Apto             ( HasAptoClient )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Models.Card             ( CardLastFour )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext(..) )

getCardLastFour
  :: (HasAccounts m, HasAptoClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> m CardLastFour
getCardLastFour trace userId = do
  CQ.getCardLastFour trace userId
