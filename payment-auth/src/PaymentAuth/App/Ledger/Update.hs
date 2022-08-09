{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}

module PaymentAuth.App.Ledger.Update
  ( UpdateLedgerExceptions(..)
  , FactTuple
  , patriotSettlmentJournal
  , createLedgerTransaction
  ) where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( fromText )
import           PaymentAuth.Monad.Ledger      as L
                                                ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( roundUpUSD )
import           Shared.Models.Ids              ( JournalId(JournalId)
                                                , LedgerEntryId(..)
                                                , LedgerTrxId(LedgerTrxId)
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..)
                                                , LedgerIdempotencyKey
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( LedgerJournal(..) )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction(..)
                                                , TransactionEntries(..)
                                                )
import           Shared.Utils.Retry             ( retryFnJitter )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

type FactTuple = (UserID, Maybe LedgerIdempotencyKey, LedgerFact)

data FactChanger
  = NormalChange LedgerFact
  | InvertedChange LedgerFact
  deriving (Eq, Show)

incrementEntry
  :: TraceContext
  -> LedgerEntryId
  -> FactChanger
  -> Maybe LedgerIdempotencyKey
  -> UTCTime
  -> LedgerEntry
  -> LedgerTrxId
  -> LedgerEntry
incrementEntry trace entryId fact idem now old@LedgerEntry {..} trxId = old
  { lenId             = entryId
  , lenRevision       = lenRevision + 1
  , lenMsgSource      = traceToMID trace
  , lenBalance        = roundUpUSD $ case fact of
                          NormalChange (TrxAdjustment _ curr) -> lenBalance + curr
                          NormalChange (PaymentCleared _ curr) -> lenBalance + curr
                          NormalChange (Manual curr) -> lenBalance + curr
                          NormalChange (InitialBalance curr) -> lenBalance + curr
                          NormalChange (UserTransfer curr) -> lenBalance + curr
                          InvertedChange (TrxAdjustment _ curr) -> lenBalance - curr
                          InvertedChange (PaymentCleared _ curr) -> lenBalance - curr
                          InvertedChange (Manual curr) -> lenBalance - curr
                          InvertedChange (InitialBalance curr) -> lenBalance - curr
                          InvertedChange (UserTransfer curr) -> lenBalance - curr
  , lenPendingBalance = lenPendingBalance
  , lenFact           = case fact of
    NormalChange   (TrxAdjustment  t curr) -> TrxAdjustment t curr
    NormalChange   (PaymentCleared p curr) -> PaymentCleared p curr
    NormalChange   (Manual         curr  ) -> Manual curr
    NormalChange   (InitialBalance curr  ) -> InitialBalance curr
    NormalChange   (UserTransfer   curr  ) -> UserTransfer curr
    InvertedChange (TrxAdjustment  t curr) -> TrxAdjustment t $ curr * (-1)
    InvertedChange (PaymentCleared p curr) -> PaymentCleared p $ curr * (-1)
    InvertedChange (Manual         curr  ) -> Manual $ curr * (-1)
    InvertedChange (InitialBalance curr  ) -> InitialBalance $ curr * (-1)
    InvertedChange (UserTransfer   curr  ) -> UserTransfer $ curr * (-1)
  , lenIdempotency    = idem
  , lenCreatedAt      = now
  , lenTransaction    = Just trxId
  }

patriotSettlmentJournal :: JournalId
patriotSettlmentJournal =
  case fromText "00000000-0000-0000-0000-000000000000" of
    Nothing -> error "not a uuid"
    Just u  -> JournalId u

data UpdateLedgerExceptions
  = FactCantBeUsed LedgerFact
  | JournalNotFound JournalId
  | JournalEntryNotFound LedgerEntryId
  deriving (Show, Exception)

getMostRecentEntry
  :: (HasLedgerDB m, MonadIO m) => TraceContext -> JournalId -> m LedgerEntry
getMostRecentEntry trace jid = do
  journalMaybe <- getLedgerJournal trace jid
  journal      <- case journalMaybe of
    Nothing -> do
      traceError trace "Error: getMostRecentEntry JournalNotFound " jid
      throw $ JournalNotFound jid
    Just ps -> return ps

  let entryToGet = lastJournalEntry journal
  journalEntryMaybe <- getLedgerEntry trace entryToGet
  case journalEntryMaybe of
    Nothing -> do
      traceError trace
                 "Error: getMostRecentEntry JournalEntryNotFound "
                 (jid, entryToGet, journal)
      throw $ JournalEntryNotFound entryToGet
    Just le -> return le

type FromJournal = JournalId
type ToJournal = JournalId
createLedgerTransaction
  :: (HasLedgerDB m, HasRandom m, HasTime m, MonadIO m, MonadMask m)
  => TraceContext
  -> Maybe LedgerIdempotencyKey
  -> FromJournal
  -> ToJournal
  -> LedgerFact
  -> m LedgerTransaction
createLedgerTransaction trace idem fromJournalId toJournalId fact =
  let txt =
        "createLedgerTransaction retry "
          <> show fact
          <> " from "
          <> show fromJournalId
          <> " to "
          <> show toJournalId
  in  retryFnJitter trace txt $ do
        fromJournalEntry      <- getMostRecentEntry trace fromJournalId
        toJournalEntry        <- getMostRecentEntry trace toJournalId

        newLdgTrxID           <- LedgerTrxId <$> aRandomUUID
        newFromJournalEntryId <- LedgerEntryId <$> aRandomUUID
        newToJournalEntryId   <- LedgerEntryId <$> aRandomUUID
        now                   <- getCurrentTime

        let newFromJournalEntry = incrementEntry trace
                                                 newFromJournalEntryId
                                                 (InvertedChange fact)
                                                 idem
                                                 now
                                                 fromJournalEntry
                                                 newLdgTrxID

        let newToJournalEntry = incrementEntry trace
                                               newToJournalEntryId
                                               (NormalChange fact)
                                               idem
                                               now
                                               toJournalEntry
                                               newLdgTrxID

        let ledgerTransaction = LedgerTransaction
              { ltxId          = newLdgTrxID
              , ltxEntries = Entries [newToJournalEntry, newFromJournalEntry]
              , ltxCreated     = now
              , ltxUpdated     = now
              , ltxFact        = fact
              , ltxIdempotency = idem
              }

        tracePrint
          trace
          "createLedgerTransaction "
          ( fromJournalId
          , toJournalId
          , fact
          , (lenRevision newFromJournalEntry, fromJournalId)
          , (lenRevision newToJournalEntry  , toJournalId)
          )

        L.saveLedgerTransaction trace ledgerTransaction Nothing
        return ledgerTransaction
