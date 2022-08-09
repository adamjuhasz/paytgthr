module PaymentAuth.App.FS.PlaidBankAccount where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Ratio                     ( (%) )
import           Data.Text                      ( Text )
import           PaymentAuth.App.RiskManagement.UpdateWorkflow
                                                ( ExtraFacts(..)
                                                , riskWorkFlow
                                                )
import           PaymentAuth.InternalAPI.ProcessEvents.ProcessRiskEvent
                                                ( RiskChanges(..) )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..)
                                                , UserCreateFSBody(..)
                                                )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.RiskScore        ( RiskFact(..)
                                                , RiskScore(..)
                                                )
import           Shared.TgthrMessages.Base      ( AccountType )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PlaidAccountId )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )
import           System.Random                  ( randomRIO )

newtype RoutingNum = RoutingNum Text
newtype AccountNum = AccountNum Text
newtype BankName = BankName Text
newtype AccountName = AccountName Text

createFSFromPlaid
  :: ( HasPlaidDB m
     , HasAccounts m
     , HasRiskScoresDB m
     , HasTime m
     , MonadIO m
     , HasEventTracking m
     )
  => TraceContext
  -> UserID
  -> PlaidAccountId
  -> BankName
  -> AccountName
  -> RoutingNum
  -> AccountNum
  -> AccountType
  -> m [RiskChanges]
createFSFromPlaid trace uid plaidId (BankName bName) (AccountName accountname) (RoutingNum routingNum) (AccountNum accountNum) actType
  = do
    let mid = traceToMID trace
    tracePrint
      trace
      "createFSFromPlaid "
      (uid, plaidId, bName, accountname, routingNum, accountNum, actType)

    updateTokenPrimary mid uid (plaidId, routingNum, accountNum)

    RiskScore { rskTrustScore = currentRisk } <- getRiskScoreOf trace uid
    let fact = if currentRisk == 0 then InitialRisk else ChangedLinkedAcct

    tracePrint trace
               "createFSFromPlaid adjusting risk "
               (uid, currentRisk, fact)

    riskScoreMaybe     <- riskWorkFlow trace uid fact NoExtraInfo

    amountCents :: Int <- liftIO $ randomRIO (175, 225) -- in cents
    let amountDollars = Currency "USD" $ fromIntegral amountCents % 100
    let amounts       = [amountDollars]

    let body = UserCreateFSBody { bankName            = bName
                                , accountName         = accountname
                                , achRouting          = routingNum
                                , achAccount          = accountNum
                                , verified            = True
                                , verificationAmounts = amounts
                                , accountType         = actType
                                }

    tracePrint trace "createFSFromPlaid setBankDetails " (uid, body)
    setBankDetails trace uid body

    let riskEvent = case riskScoreMaybe of
          Nothing        -> []
          Just riskScore -> [RiskUpdated riskScore]

    return riskEvent
