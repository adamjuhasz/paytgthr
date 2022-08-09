{- HLINT ignore "Reduce duplication" -}

module PaymentAuth.App.Payments.UpdatePayment where

import           Control.Exception              ( Exception
                                                , SomeException
                                                , throw
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadMask
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Text                      ( Text )
import           PaymentAuth.App.Ledger.CreateFacts
                                                ( createPaymentFact )
import           PaymentAuth.App.Ledger.CreateJournal
                                                ( createNewJournal )
import           PaymentAuth.App.Ledger.Update  ( createLedgerTransaction )
import           PaymentAuth.App.Payments.Effects
                                                ( PaymentEffects(..) )
import           PaymentAuth.App.Payments.MakeAPayment
                                                ( getUsersJournal
                                                , makePayments
                                                )
import           PaymentAuth.App.RiskManagement.UpdateWorkflow
                                                ( ExtraFacts(..)
                                                , riskWorkFlow
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( CreateLedgerJournalBody(..)
                                                , HasLedgerDB(..)
                                                )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Ids              ( JournalId(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(lenJournal) )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(FundingSource)
                                                , LedgerJournal(..)
                                                , PaymentMethod(DwollaACH)
                                                )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction(ltxEntries)
                                                , TransactionEntries
                                                  ( Entries
                                                  , EntriesById
                                                  )
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentId
                                                , PaymentStatus(..)
                                                , PaymentSubType(..)
                                                , PaymentType(CreditToUser)
                                                )
import           Shared.Models.RiskScore        ( RiskFact(..) )
import           Shared.Models.User             ( RedactedText )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data UpdatePaymentErrors = TransitionNotAllowed PaymentStatus PaymentStatus
  deriving Show
instance Exception UpdatePaymentErrors

newtype PayWas = PayWas PaymentStatus deriving (Eq, Show)
newtype PayIs = PayIs PaymentStatus deriving (Eq, Show)
data PaymentWasVisible = PaymentIsVisible | PaymentIsHidden deriving (Eq, Show)

updateLedgerForPayment
  :: (HasLedgerDB m, HasRandom m, HasTime m, MonadIO m, MonadMask m)
  => TraceContext
  -> Payment
  -> m (LedgerEntry, LedgerTransaction)
updateLedgerForPayment trace payment = do
  let user = payUser payment
  LedgerJournal { journalId = ptJournalId } <- getUsersJournal trace
    $ GetPayTgthr user

  fsJournalMaybe <- getLedgerJournalType trace $ GetFundingSource user
  fsJournalId    <- case fsJournalMaybe of
    [] -> do
      jid <- JournalId <$> aRandomUUID
      let jType =
            FundingSource user (DwollaACH "UNKNOWN - updateLedger created")
      createNewJournal
        trace
        CreateLedgerJournalBody
          { newJournalId   = jid
          , newJournalType = jType
          , newJournalName = "Emergency FS made by UpdatePayment.updateLedger"
          , startBalance   = Currency "USD" 0
          }
      return jid
    lj : _ -> return $ journalId lj

  let (_, idem, fact) = createPaymentFact payment

  let (fromJournal, toJournal) =
        case (payFromJournal payment, payToJournal payment) of
          (Just from, Just to) -> (from, to)
          (_        , _      ) -> (fsJournalId, ptJournalId)

  ledgerTransaction <- createLedgerTransaction trace
                                               idem
                                               fromJournal
                                               toJournal
                                               fact

  let userEntries = case ltxEntries ledgerTransaction of
        EntriesById _ -> error $ "Error: need full entries " <> show
          (ledgerTransaction, user, trace)
        Entries les -> les

  let isUserEntry = (==) toJournal . lenJournal

  let
    userEntry = case filter isUserEntry userEntries of
      [] -> error $ "Error: no entries found for " <> show
        (userEntries, user, trace)
      [ le] -> le
      _ : _ -> error $ "Error: multiple entries found for " <> show
        (userEntries, user, trace)

  return (userEntry, ledgerTransaction)

updatePayment
  :: ( HasPaymentsDB m
     , HasLedgerDB m
     , HasRandom m
     , HasTime m
     , HasAccounts m
     , HasRiskScoresDB m
     , MonadIO m
     , HasEventTracking m
     , MonadMask m
     )
  => TraceContext
  -> PaymentId
  -> PaymentStatus
  -> Maybe Text
  -> Maybe (RedactedText, RedactedText)
  -> m [PaymentEffects]
updatePayment trace newPayId newPayState methodId achInfo = do
  let mid = traceToMID trace
  payment <- fromJust <$> getPayment trace newPayId
  let newACHInfo = if isJust achInfo then achInfo else payACHInfo payment
  let updatedPayment = payment { payMsgSource = mid
                               , payStatus    = newPayState
                               , payMethodId  = methodId
                               , payRevision  = 1 + payRevision payment
                               , payACHInfo   = newACHInfo
                               }

  let user   = payUser payment
  let method = payMethod payment

  let saveNewPayment aPayment = do
        savePayment trace aPayment
        return [PaymentWasUpdated (payStatus payment) aPayment]


  let currentState = payStatus payment
  evts <- case (currentState, newPayState) of
    (PaymentCreated, _) -> saveNewPayment updatedPayment
    (PaymentPending, PaymentCreated) ->
      throw $ TransitionNotAllowed currentState newPayState
    (PaymentPending, PaymentPending) ->
      throw $ TransitionNotAllowed currentState newPayState
    (PaymentPending  , PaymentCompleted) -> saveNewPayment updatedPayment
    (PaymentPending  , PaymentFailed _ ) -> saveNewPayment updatedPayment
    (PaymentPending  , PaymentCancelled) -> saveNewPayment updatedPayment
    (PaymentCompleted, PaymentFailed _ ) -> saveNewPayment updatedPayment
    (PaymentCompleted, _) ->
      throw $ TransitionNotAllowed currentState newPayState
    (PaymentFailed _, _) ->
      throw $ TransitionNotAllowed currentState newPayState
    (PaymentCancelled, _) ->
      throw $ TransitionNotAllowed currentState newPayState

  let riskEventFactory Nothing          = []
      riskEventFactory (Just riskScore) = [RiskWasUpdated riskScore]

  let ledgerEventFactory ledgerFact = [LedgerWasUpdated ledgerFact]

  let adjustRiskForFailed = do
        tracePrint trace
                   "UpdatePayment _ -> PaymentFailed "
                   (user, method, mid, updatedPayment)
        riskScoreMaybe <- riskWorkFlow trace
                                       (payUser payment)
                                       (FailedPayment (payId payment))
                                       (PaymentInfo payment)
        return $ riskEventFactory riskScoreMaybe

  let adjustRiskForCompleted = do
        riskScoreMaybe <- riskWorkFlow trace
                                       (payUser payment)
                                       (SuccessfulPayment (payId payment))
                                       (PaymentInfo payment)
        return $ riskEventFactory riskScoreMaybe

  let oldState = PayWas $ payStatus payment       -- Old state
  let newState = PayIs $ payStatus updatedPayment -- Current state

  let handler
        :: (MonadIO m) => SomeException -> m (LedgerEntry, LedgerTransaction)
      handler e = do
        traceError
          trace
          "Error: updatePayment updateLedger "
          (e, newPayId, newPayState, methodId, achInfo, oldState, newState)
        throw e

  let ledgerUpdater x = updateLedgerForPayment trace x `catch` handler

  causedEvts <- case (paySubType payment, oldState, newState) of
    (NormalPayment, PayWas PaymentCreated, PayIs PaymentCompleted) -> do
      traceError
        trace
        "Error: PayCmd UpdatePayment NormalPayment PaymentCreated -> PaymentCompleted"
        (user, method, mid, updatedPayment)

      -- ledger update
      (ledgerFact, _) <- ledgerUpdater updatedPayment
      let ledgerEvent = ledgerEventFactory ledgerFact

      -- risk update
      riskEvent <- adjustRiskForCompleted

      -- return 
      return $ ledgerEvent <> riskEvent
    (NormalPayment, PayWas PaymentPending, PayIs PaymentCompleted) -> do
      tracePrint
        trace
        "UpdatePayment NormalPayment PaymentPending -> PaymentCompleted update "
        (user, method, updatedPayment)
      -- ledger update
      (ledgerFact, _) <- ledgerUpdater updatedPayment
      let ledgerEvent = ledgerEventFactory ledgerFact

      -- risk update
      riskEvent <- adjustRiskForCompleted

      -- return 
      return $ ledgerEvent <> riskEvent
    (NormalPayment, PayWas PaymentCompleted, PayIs (PaymentFailed _)) -> do
      traceError
        trace
        "Error: PayCmd NormalPayment UpdatePayment PaymentCompleted -> PaymentFailed"
        (user, method, mid, updatedPayment)

      -- Update ledger
      (ledgerFact, _) <- ledgerUpdater updatedPayment
      let ledgerEvent = ledgerEventFactory ledgerFact

      -- update risk score
      riskEvent <- adjustRiskForFailed

      --return
      return $ ledgerEvent <> riskEvent
    (NormalPayment, PayWas _, PayIs (PaymentFailed _)) -> adjustRiskForFailed
    (NormalPayment, PayWas a, PayIs b                ) -> do
      tracePrint trace
                 "UpdatePayment NormalPayment Noop Payment update  "
                 (user, method, mid, updatedPayment, a, b)
      return []

    -- Verification deposits
    (InitialVerification, PayWas _, PayIs PaymentCompleted) -> do
      tracePrint trace
                 "UpdatePayment InitialVerification _ -> PaymentCompleted "
                 (user, method, mid, CreditToUser)

      makePayments trace
                   (payUser updatedPayment)
                   [(RefundVerification, abs $ payAmount updatedPayment)] -- positive amount for credit payment

    (InitialVerification, PayWas _, PayIs (PaymentFailed reason)) -> do
      tracePrint trace
                 "UpdatePayment InitialVerification _ -> PaymentFailed "
                 (user, method, mid, updatedPayment)

      _ <- removeFundingSource trace user method (Just reason)
      return []

    (InitialVerification, PayWas _, PayIs _) -> do
      tracePrint trace
                 "UpdatePayment InitialVerification Noop Payment update "
                 (user, method, mid, updatedPayment)
      return []

    (RefundVerification, PayWas _, PayIs (PaymentFailed reason)) -> do
      tracePrint trace
                 "UpdatePayment RefundVerification _ -> PaymentFailed "
                 (user, method, mid, updatedPayment)

      _ <- removeFundingSource trace user method (Just reason)
      return []

    (RefundVerification, PayWas _, PayIs _) -> do
      tracePrint trace
                 "UpdatePayment RefundVerification Noop Payment update "
                 (user, method, mid, updatedPayment)
      return []

  return $ evts <> causedEvts
