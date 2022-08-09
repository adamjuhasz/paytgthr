module PaymentAuth.App.RiskManagement.UpdateWorkflow where

import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking
                                                  ( trackEventWithProps
                                                  )
                                                )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Models.Ids              ( UserID(..) )
import           Shared.Models.Payment          ( Payment(..) )
import           Shared.Models.RiskScore        ( RiskFact(..)
                                                , RiskScore(..)
                                                , UserLevel(..)
                                                , getUserLevel
                                                , riskAdvancer
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data ExtraFacts
  = PaymentInfo Payment
  | NoExtraInfo
  deriving (Eq, Show)

type RiskGetter = UserID -> IO RiskScore
type RiskSetter = RiskScore -> IO ()

  -- inline brittany config for width
  -- brittany-next-binding --columns 130
riskWorkFlow
  :: (HasRiskScoresDB m, HasTime m, HasAccounts m, HasEventTracking m)
  => TraceContext
  -> UserID
  -> RiskFact
  -> ExtraFacts
  -> m (Maybe RiskScore)
riskWorkFlow trace uid fact xtra = do
  let mid = traceToMID trace
  currentScore <- getRiskScoreOf trace uid
  now          <- getCurrentTime

  let initialRiskValue = 10
  let userLevel        = getUserLevel currentScore
  let riskChange = case fact of
        InitialRisk             -> const initialRiskValue
        (ManualRiskAdj     val) -> const val
        (SuccessfulPayment _  ) -> case xtra of
          PaymentInfo Payment { payAmount = amount } -> (+) (riskAdvancer amount userLevel)
          NoExtraInfo -> (+) 0
        (FailedPayment _)    -> (+) 0
        ChangedLinkedAcct    -> (+) 0
        (RiskyAcctBalance _) -> (+) 0
        (RiskyTransaction _) -> (+) 0

  let newScore = min 100 . max 0 $ riskChange (rskTrustScore currentScore)
  let score' = currentScore { rskRev        = rskRev currentScore + 1
                            , rskTrustScore = newScore
                            , rskChange     = newScore - rskTrustScore currentScore
                            , rskFact       = fact
                            , rskMsgSource  = mid
                            , rskCreatedAt  = now
                            }
  saveRisk trace score'

  let lockCards tid = do
        adminlockCardsForUser trace uid
        trackEventWithProps trace uid "Cards AdminLocked RiskyTransaction"
          $ object ["transaction" .= tid, "riskScore" .= rskTrustScore currentScore]

  -- mitigte risky behavior
  case (userLevel, fact) of
    (Level0, RiskyTransaction tid) -> lockCards tid
    (Level1, RiskyTransaction tid) -> lockCards tid
    (_     , _                   ) -> return ()


  if newScore == rskTrustScore currentScore
    then return Nothing -- no change
    else return $ Just score'
