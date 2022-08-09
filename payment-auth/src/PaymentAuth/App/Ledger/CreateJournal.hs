{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.Ledger.CreateJournal where

import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Text                     as T
import           PaymentAuth.Monad.Ledger       ( CreateLedgerJournalBody(..)
                                                , HasLedgerDB(..)
                                                )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Ids              ( LedgerEntryId(LedgerEntryId)
                                                , LedgerTrxId(LedgerTrxId)
                                                , UserID(UserID)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalType(..)
                                                , journalOwner
                                                , searchFor
                                                )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction(..)
                                                , TransactionEntries(Entries)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

checkSingleton :: HasLedgerDB m => TraceContext -> JournalType -> m Bool
checkSingleton trace newJournalType = do
  let singletonSearch = case newJournalType of
        ExternalAccount _ -> Nothing
        VirtualAccount    -> Nothing
        njt               -> Just $ searchFor njt

  case singletonSearch of
    Nothing -> return False
    Just s  -> do
      existingJ <- getLedgerJournalType trace s
      case existingJ of
        []    -> return False
        _ : _ -> return True

createNewJournal
  :: (HasLedgerDB m, HasTime m, HasRandom m, MonadIO m)
  => TraceContext
  -> CreateLedgerJournalBody
  -> m ()
createNewJournal trace body@CreateLedgerJournalBody {..} = do
  now     <- getCurrentTime

  entryId <- LedgerEntryId <$> aRandomUUID
  trxId   <- LedgerTrxId <$> aRandomUUID

  tracePrint trace "createNewJournal " (body, entryId, trxId)

  let theFact = InitialBalance startBalance
  let idem    = Just . T.pack $ show newJournalId

  let jOwner  = UserID <$> journalOwner newJournalType

  singletonExisits <- checkSingleton trace newJournalType
  when singletonExisits $ do
    traceError trace
               "createNewJournal Journal Type already exists for user"
               body
    error
      $  "createNewJournal Journal Type already exists for user "
      <> show body

  let firstEntry = LedgerEntry { lenId             = entryId
                               , lenJournal        = newJournalId
                               , lenUser           = jOwner
                               , lenRevision       = 1
                               , lenVersion        = "1.0"
                               , lenMsgSource      = traceToMID trace
                               , lenBalance        = startBalance
                               , lenPendingBalance = Currency "USD" 0
                               , lenFact           = theFact
                               , lenIdempotency    = idem
                               , lenCreatedAt      = now
                               , lenTransaction    = Just trxId
                               }

  tracePrint trace "createNewJournal firstEntry " firstEntry

  let firstTrx = LedgerTransaction { ltxId          = trxId
                                   , ltxEntries     = Entries [firstEntry]
                                   , ltxCreated     = now
                                   , ltxUpdated     = now
                                   , ltxFact        = theFact
                                   , ltxIdempotency = idem
                                   }

  tracePrint trace "createNewJournal firstTrx " firstTrx

  saveLedgerTransaction trace firstTrx $ Just body

  return ()
