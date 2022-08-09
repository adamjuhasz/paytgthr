module PaymentAuth.App.GetRisk where

import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB
                                                  ( getRiskScoreOf
                                                  )
                                                )
import           Shared.Models.RiskScore        ( RiskScore )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )

getRiskFor :: (HasRiskScoresDB m) => TraceContext -> UserID -> m RiskScore
getRiskFor = getRiskScoreOf
