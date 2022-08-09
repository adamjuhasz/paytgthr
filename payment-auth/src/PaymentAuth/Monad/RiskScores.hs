module PaymentAuth.Monad.RiskScores where

import           Shared.Models.RiskScore        ( RiskScore )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasRiskScoresDB m where
  getRiskScoreOf :: TraceContext -> UserID -> m RiskScore
  saveRisk       :: TraceContext -> RiskScore -> m ()
