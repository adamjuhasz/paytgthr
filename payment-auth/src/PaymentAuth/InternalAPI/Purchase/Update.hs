{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE NamedFieldPuns #-}
{- HLINT ignore "Reduce duplication" -}

module PaymentAuth.InternalAPI.Purchase.Update where

import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO )
import           PaymentAuth.App.Payments.MakeAPayment
                                                ( payOutLedger )
import qualified PaymentAuth.App.Purchases.UpdateTransaction
                                               as PAuth
import           PaymentAuth.App.Purchases.UpdateTrx
                                                ( UpdatedTrxInfo(..) )
import           PaymentAuth.InternalAPI.Payments.ProcessEffects
                                                ( processPaymentEffects )
import           PaymentAuth.InternalAPI.ProcessEvents.ProcessLedgerEvent
                                                ( processLedgerEntry )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.Time         ( HasTime )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionId(..)
                                                , TransactionState(..)
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.PaymentAuth.API  ( TraceContext
                                                , UpdatePurchaseBody(..)
                                                )

updatePurchase
  :: ( HasAccounts m
     , HasDwollaClient m
     , HasEventTracking m
     , HasLedgerDB m
     , HasPaymentsDB m
     , HasRandom m
     , HasTime m
     , HasTransactionsDB m
     , MonadIO m
     , MonadMask m
     )
  => TraceContext
  -> TransactionId
  -> UpdatePurchaseBody
  -> m NoContent
updatePurchase trace trxId UpdatePurchaseBody {..} = do
  let updates = UpdatedTrxInfo { utiTransaction      = trxId
                               , utiSource           = source
                               , utiIdempotency      = idempotency
                               , utiSourceId         = sourceId
                               , utiDetails          = details
                               , utiMerchant         = merchant
                               , utiState            = state
                               , utiCreatedAt        = createdAt
                               , utiTransactionEvent = transactionEvent
                               , utiAmountLocal      = amountLocal
                               , utiAmountHold       = amountHold
                               , utiAmountCashback   = amountCashback
                               , utiAmountFee        = amountFee
                               , utiAmountBilling    = amountBilling
                               , utiDescription      = description
                               , utiAdjustments      = adjustments
                               }

  (prevTrxState, updatedTrx, ledgerEntrys) <-
    retryFn trace ("PAuth.updateTransaction " <> show trxId)
      $ PAuth.updateTransaction purchaser trace updates

  mapM_ (processLedgerEntry trace) ledgerEntrys
  processTransactionChange trace prevTrxState updatedTrx

  return NoContent

type PreviousTransactionState = TransactionState

processTransactionChange
  :: ( HasAccounts m
     , HasDwollaClient m
     , HasEventTracking m
     , HasLedgerDB m
     , HasPaymentsDB m
     , HasRandom m
     , HasTime m
     , MonadIO m
     )
  => TraceContext
  -> PreviousTransactionState
  -> Transaction
  -> m ()
processTransactionChange _ (TrxDeclined _) Transaction { trxState = TrxDeclined _ }
  = return ()
processTransactionChange trace prevState trx@Transaction { trxState = TrxCompleted }
  = do
    trackTransactionChanged trace prevState trx
    -- Realtime ACH payments, charge a user for each payment right as it completes
    let affected = fmap fst . trxSplitAmounts $ trx
    evts <- concat <$> mapM (payOutLedger trace) affected
    mapM_ (processPaymentEffects trace) evts

processTransactionChange trace prevState trx =
  trackTransactionChanged trace prevState trx
