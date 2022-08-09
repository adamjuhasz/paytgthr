{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE RecordWildCards, StrictData #-}

module PaymentAuth.App.GetBalance where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Maybe                     ( listToMaybe )
import           PaymentAuth.App.Split          ( calculateSplit )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..)
                                                , IncludeVerificationPayments(..)
                                                )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Currency         ( Currency(..)
                                                , getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Ledger.Journal   ( JournalSearch(GetPayTgthr)
                                                , LedgerJournal(journalBalance)
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentSubType(..)
                                                , PaymentType(..)
                                                )
import           Shared.Models.RiskScore        ( riskAdjustedLimit )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionState(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

foldCurrency :: [Currency] -> Currency
foldCurrency pays = foldr (+) (Currency isoCurr 0) pays
  where isoCurr = maybe "USD" getIsoCode (listToMaybe pays)

foldPayment :: [Payment] -> Currency
foldPayment = foldCurrency . fmap getSign
 where
  getSign Payment { paySubType = InitialVerification } = 0
  getSign Payment { paySubType = RefundVerification }  = 0
  getSign Payment { payType = CreditToUser, ..}        = payAmount * (-1)
  getSign Payment { payType = DebitFromUser }          = 0 -- debits can fail

getLiability
  :: (HasLedgerDB m, HasTransactionsDB m, HasPaymentsDB m)
  => TraceContext
  -> UserID
  -> m Currency
getLiability trace uid = do
  -- add up all the current pending ACH payments
  paymentTotal <-
    foldPayment <$> getPendingPaymentsOf trace uid WithoutVerifcication
  ledgerJournalM <- listToMaybe <$> getLedgerJournalType trace (GetPayTgthr uid)
  pendingTrxs    <- getPendingTransactionsFor trace uid

  let ledgerBalance = maybe (Currency "USD" 0) journalBalance ledgerJournalM
  let portionResponsibleFor Transaction { trxState = state, ..}
        | state == TrxAuthorized || state == TrxPending = calculateSplit
          (getMonetaryValue trxDisplayAmount)
          trxSplitAmounts
        | otherwise = []

  -- add up all the current pending transactions (not recorded into
  let splitAmounts = concatMap portionResponsibleFor pendingTrxs
  let usersOwed              = filter (\(u, _) -> u == uid) splitAmounts
  let owedCurrency = fmap (\(_, val) -> Currency "USD" val) usersOwed
  let transactionTotal       = foldCurrency owedCurrency

  let normalizedPayments     = paymentTotal
  -- flip trx sign b/c negative is user owes us
  let normalizedTransactions = (-1) * transactionTotal

  return (normalizedPayments + normalizedTransactions + ledgerBalance)

getSpendingLimit
  :: ( HasLedgerDB m
     , HasTransactionsDB m
     , HasPaymentsDB m
     , HasRiskScoresDB m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> m Currency
getSpendingLimit trace uid = do
  liability  <- getLiability trace uid
  trustScore <- getRiskScoreOf trace uid

  let riskBasedLimit = riskAdjustedLimit trustScore
  let netBalance     = riskBasedLimit + liability

  tracePrint trace
             "getSpendingLimit (uid, riskBasedLimit, netBalance)"
             (uid, riskBasedLimit, netBalance)

  return netBalance
