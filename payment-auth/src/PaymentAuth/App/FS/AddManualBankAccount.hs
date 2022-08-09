{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.FS.AddManualBankAccount where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( unless )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           PaymentAuth.App.RiskManagement.UpdateWorkflow
                                                ( ExtraFacts(..)
                                                , riskWorkFlow
                                                )
import           PaymentAuth.InternalAPI.ProcessEvents.ProcessRiskEvent
                                                ( RiskChanges(..) )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..)
                                                , IncludeVerificationPayments(..)
                                                )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentStatus(..)
                                                , PaymentSubType(..)
                                                )
import           Shared.Models.RiskScore        ( RiskFact(..) )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )
import           System.Random                  ( randomRIO )

type ABARoutong = Text
type DDAAccount = Text
data SetManualBankErrors
  = PendingVerificiationPayments ABARoutong DDAAccount
  | CantFindUser UserID
  deriving Show
instance Exception SetManualBankErrors

setManualBankAccount
  :: ( HasPaymentsDB m
     , HasAccounts m
     , HasRiskScoresDB m
     , HasTime m
     , MonadIO m
     , HasEventTracking m
     )
  => TraceContext
  -> UserID
  -> Text
  -> Text
  -> m ([Double], [RiskChanges])
setManualBankAccount trace smcUser smcABARouting smcDDAAccount = do
  pendingPays <- getPendingPaymentsOf trace smcUser IncludeVerification
  let pendingHiddenDebits = filter
        (\Payment {..} ->
          paySubType == InitialVerification && payStatus == PaymentPending
        )
        pendingPays
  unless
    (null pendingHiddenDebits)
    (do
      let maskedDDA = "****" <> T.takeEnd 4 smcDDAAccount
      tracePrint
        trace
        "Trying to setManualBankAccount with pending hidden payments "
        (smcUser, smcABARouting, maskedDDA)
      throw $ PendingVerificiationPayments smcABARouting maskedDDA
    )

  -- generate random amount $1.75 -> $2.25, positive for credit
  amountCents :: Int <- liftIO $ randomRIO (175, 225) -- in cents
  let amountDollars :: Double = fromIntegral amountCents / 100
  let amounts                 = [amountDollars]

  userMaybe <- getUser trace smcUser
  user      <- case userMaybe of
    Nothing -> do
      traceError trace "Error: Can't get user: " smcUser
      throw $ CantFindUser smcUser
    Just u -> return u

  let fact = case usrBankAcount user of
        Nothing -> InitialRisk
        Just _  -> ChangedLinkedAcct

  riskScoreMaybe <- riskWorkFlow trace smcUser fact NoExtraInfo

  let riskChanges = case riskScoreMaybe of
        Nothing        -> []
        Just riskScore -> [RiskUpdated riskScore]

  return (amounts, riskChanges)


