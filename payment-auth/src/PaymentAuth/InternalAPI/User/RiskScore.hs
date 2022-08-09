module PaymentAuth.InternalAPI.User.RiskScore where

import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB
                                                  ( getRiskScoreOf
                                                  )
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.RiskScore        ( RiskScore )
import           Shared.WebAPI.General.API      ( TraceContext )

getRiskScore :: (HasRiskScoresDB m) => TraceContext -> UserID -> m RiskScore
getRiskScore = getRiskScoreOf
