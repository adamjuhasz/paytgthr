{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module PaymentAuth.App.Purchases.Workflow.Update.UpdateLedger
  ( updateLedgerWorkflow
  ) where

import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Purchases.UpdateLedger
                                                ( updateLedgerAsNeeded )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalType(..) )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionState(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

updateLedgerWorkflow
  :: ( HasAccounts m
     , HasLedgerDB m
     , HasTime m
     , HasRandom m
     , MonadIO m
     , MonadMask m
     )
  => TraceContext
  -> Transaction
  -> m [(LedgerEntry, LedgerTransaction)]
updateLedgerWorkflow trace updatedTrx = do
  let affectedUsers = fst <$> trxSplitAmounts updatedTrx

  let updateLedger = do
        let journals = PayTgthr <$> affectedUsers
        tracePrint trace "Making ledger entries for " (journals, updatedTrx)
        concat <$> mapM (updateLedgerAsNeeded trace updatedTrx) journals

  -- checking existing subs
  case trxState updatedTrx of
    TrxCreated         -> return []
    TrxAuthorized      -> return []
    TrxPending         -> return []
    TrxDeclined _      -> return []
    TrxCompleted       -> updateLedger
    TrxPendingReversal -> updateLedger
    TrxDisputed _      -> updateLedger
