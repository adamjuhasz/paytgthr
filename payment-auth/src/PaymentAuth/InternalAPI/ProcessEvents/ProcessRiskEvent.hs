module PaymentAuth.InternalAPI.ProcessEvents.ProcessRiskEvent where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Models.RiskScore        ( RiskScore )

newtype RiskChanges
  = RiskUpdated RiskScore
  deriving Show

processRiskEvents :: (MonadIO m) => RiskChanges -> m ()
processRiskEvents r = do
  liftIO $ print r
  return ()
