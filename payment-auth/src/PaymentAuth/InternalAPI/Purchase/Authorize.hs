{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.InternalAPI.Purchase.Authorize where

import           Control.Monad.Catch            ( MonadCatch(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Purchases.AuthorizeTransaction
                                                ( authorizeNewTransaction )
import           PaymentAuth.App.Purchases.CreateTransaction
                                                ( createTransaction )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Transaction      ( Transaction
                                                  ( Transaction
                                                  , trxState
                                                  )
                                                , TransactionEvent(AuthRequest)
                                                , TransactionState(..)
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult
                                                , extractTrxFromAuthResult
                                                )
import           Shared.WebAPI.PaymentAuth.API  ( AuthorizePurchaseBody(..)
                                                , TraceContext
                                                , traceToMID
                                                )

autorizeTransaction
  :: ( HasRiskScoresDB m
     , HasTime m
     , HasAccounts m
     , HasTransactionsDB m
     , HasLedgerDB m
     , HasPaymentsDB m
     , MonadIO m
     , HasEventTracking m
     , MonadCatch m
     )
  => TraceContext
  -> UserID
  -> AuthorizePurchaseBody
  -> m AuthResult
autorizeTransaction trace userId AuthorizePurchaseBody {..} = do
  now <- getCurrentTime
  let mid = traceToMID trace
  let newTransaction = createTransaction mid
                                         transactionId
                                         TrxAuthorized
                                         AuthRequest
                                         amount
                                         userId
                                         Nothing
                                         source
                                         sourceId
                                         details
                                         merchant
                                         (Just description)
                                         now

  (authResult, prevTrx) <- authorizeNewTransaction trace cardUsed newTransaction

  let trx = extractTrxFromAuthResult authResult
  case prevTrx of
    Nothing -> trackTransactionChanged trace TrxCreated trx
    Just Transaction { trxState = prevState } ->
      trackTransactionChanged trace prevState trx

  return authResult
