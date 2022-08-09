{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.DB.Ledger where

import           Control.Monad                  ( forM_ )
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Data.Time.Clock                ( getCurrentTime )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(Only)
                                                , execute
                                                , query
                                                , query_
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( JournalId
                                                , LedgerEntryId
                                                , LedgerTrxId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerIdempotencyKey )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..)
                                                , ledgerEntryFields
                                                )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(..)
                                                , LedgerJournal(..)
                                                , journalSearchToTag
                                                , ledgerJournalFields
                                                )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction(..)
                                                , TransactionEntries(..)
                                                , ledgerTransactionlFields
                                                , verifyTransactionIsSound
                                                )
import           Shared.WebAPI.PaymentAuth.API  ( CreateLedgerJournalBody(..) )

getLedgerEntryForIdemKey
  :: JournalId -> LedgerIdempotencyKey -> Connection -> IO (Maybe LedgerEntry)
getLedgerEntryForIdemKey jid idem conn = listToMaybe
  <$> query conn withJournalSearch (idem, jid)
 where
  withJournalSearch =
    "SELECT "
      <> fst ledgerEntryFields
      <> " FROM tgthr.ledger WHERE idempotency = ? AND journal_id = ?"

saveLedgerEntry :: LedgerEntry -> Connection -> IO ()
saveLedgerEntry entry conn = withTransaction conn $ do
  _ <- execute conn insert entry
  _ <- execute conn upsert entry
  return ()
 where
  insert =
    "INSERT INTO tgthr.ledger ("
      <> fst ledgerEntryFields
      <> ") VALUES ("
      <> snd ledgerEntryFields
      <> ")"
  upsert =
    "UPSERT INTO tgthr.ledger_current_rev ("
      <> fst ledgerEntryFields
      <> ") VALUES ("
      <> snd ledgerEntryFields
      <> ")"

getUsersWithBalances :: Connection -> IO [(UserID, Currency)]
getUsersWithBalances conn = query_ conn qs
  -- SELECT Pay Tgthr Journals WHERE balance NOT 0.0 
  -- FILTER users who are not active
  -- FILTER users who are not FS verified
 where
  qs = [sql| 
      SELECT tgthr.journals_current_rev.user_id, tgthr.journals_current_rev.balance 
      FROM tgthr.journals_current_rev 
      INNER JOIN tgthr.users_current_rev ON tgthr.journals_current_rev.user_id = tgthr.users_current_rev.id 
      WHERE 
          journals_current_rev.user_id IS NOT NULL 
          AND journals_current_rev.kind = 'PayTgthr'
          AND journals_current_rev.balance_number != 0 
          AND users_current_rev.status = 'active'
          AND users_current_rev.bank_verified = true
      |]

getMostRecentJournal :: JournalId -> Connection -> IO (Maybe LedgerJournal)
getMostRecentJournal jid conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE id = ?"
  selector = Only jid

saveLedgerTransaction
  :: LedgerTransaction -> Maybe CreateLedgerJournalBody -> Connection -> IO ()
saveLedgerTransaction trx@LedgerTransaction { ltxEntries = EntriesById{} } _ _
  = error $ "Error: Can not save transaction with EntriesById " <> show trx
saveLedgerTransaction trx@LedgerTransaction { ltxEntries = Entries [] } _ _ =
  error $ "Error: Can not save transaction with no entries " <> show trx
saveLedgerTransaction trx@LedgerTransaction { ltxEntries = Entries entries, ..} journalDefault conn
  = withTransaction conn $ do
    let verified = verifyTransactionIsSound trx

    _ <- case verified of
      Right _ -> return ()
      Left  t -> error t

    putStr "saveLedgerTransaction saving transaction " >> print trx

    -- save each entry
    forM_ entries $ \entry@LedgerEntry {..} -> do
      _ <- execute conn insertEntry entry
      _ <- execute conn upsertEntry entry

      putStr "saveLedgerTransaction saved " >> print (lenId, entry)

      journalMaybe <- lenJournal `getMostRecentJournal` conn

      putStr "saveLedgerTransaction journal read "
        >> print (lenId, journalMaybe)

      now <- getCurrentTime
      let journal = case (journalMaybe, journalDefault) of
            (Nothing, Nothing  ) -> Nothing
            (Nothing, Just body) -> Just $ createJournal now entry body
            (Just j , _        ) -> Just j

      putStr "saveLedgerTransaction journal to update "
        >> print (lenId, journalId <$> journal, journal)

      case journal of
        Nothing ->
          putStr "Error: saveLedgerTransaction no journal saved " >> print lenId
        Just j -> do
          let newJournal = j { journalRevision       = journalRevision j + 1
                             , journalUpdated        = now
                             , lastJournalEntry      = lenId
                             , journalBalance        = lenBalance
                             , journalPendingBalance = lenPendingBalance
                             , journalTransaction    = ltxId
                             }
          _ <- execute conn insertJournal newJournal
          _ <- execute conn upsertJournal newJournal

          putStr "saveLedgerTransaction journal saved "
            >> print (lenId, newJournal)

    _ <- execute conn insertTransaction trx
    putStr "saveLedgerTransaction transaction saved " >> print trx

    return ()
 where
  insertEntry =
    "INSERT INTO tgthr.ledger ("
      <> fst ledgerEntryFields
      <> ") VALUES ("
      <> snd ledgerEntryFields
      <> ")"
  upsertEntry =
    "UPSERT INTO tgthr.ledger_current_rev ("
      <> fst ledgerEntryFields
      <> ") VALUES ("
      <> snd ledgerEntryFields
      <> ")"
  insertJournal =
    "INSERT INTO tgthr.journals ("
      <> fst ledgerJournalFields
      <> ") VALUES ("
      <> snd ledgerJournalFields
      <> ")"
  upsertJournal =
    "UPSERT INTO tgthr.journals_current_rev ("
      <> fst ledgerJournalFields
      <> ") VALUES ("
      <> snd ledgerJournalFields
      <> ")"
  insertTransaction =
    "INSERT INTO tgthr.ledger_transaction ("
      <> fst ledgerTransactionlFields
      <> ") VALUES ("
      <> snd ledgerTransactionlFields
      <> ")"
  createJournal now LedgerEntry {..} CreateLedgerJournalBody {..} =
    LedgerJournal
      { journalId             = newJournalId
      , journalType           = newJournalType
      , journalName           = newJournalName
      , journalUser           = case newJournalType of
                                  PayTgthr        ui -> Just ui
                                  PayTgthrRewards ui -> Just ui
                                  StashTgthr      ui -> Just ui
                                  SaveTgthr       ui -> Just ui
                                  SecurtyDeposit  ui -> Just ui
                                  FundingSource ui _ -> Just ui
                                  ExternalAccount _  -> Nothing
                                  VirtualAccount     -> Nothing
      , lastJournalEntry      = lenId
      , journalBalance        = lenBalance
      , journalPendingBalance = lenPendingBalance
      , journalRevision       = 0
      , journalCreated        = now
      , journalUpdated        = now
      , journalTransaction    = ltxId
      }

getJournalType :: JournalSearch -> Connection -> IO [LedgerJournal]
getJournalType jType conn = case jType of
  GetPayTgthr        ui -> queryWithUser ui
  GetPayTgthrRewards ui -> queryWithUser ui
  GetStashTgthr      ui -> queryWithUser ui
  GetSaveTgthr       ui -> queryWithUser ui
  GetSecurtyDeposit  ui -> queryWithUser ui
  GetFundingSource   ui -> queryWithUser ui
  GetExternalAccount    -> queryWOUser
  GetVirtualAccount     -> queryWOUser
 where
  queryWithUser uid = query conn userQS (uid, journalSearchToTag jType)
  queryWOUser = query conn nonUserQS (Only $ journalSearchToTag jType)
  userQS =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE user_id = ? AND kind = ? ORDER BY created_at DESC "
  nonUserQS =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE kind = ? ORDER BY created_at DESC "

getJournalId :: JournalId -> Connection -> IO (Maybe LedgerJournal)
getJournalId jid conn = listToMaybe <$> query conn qs (Only jid)
 where
  qs =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE id = ? "

getLedgerEntryId :: LedgerEntryId -> Connection -> IO (Maybe LedgerEntry)
getLedgerEntryId leid conn = listToMaybe <$> query conn qs (Only leid)
 where
  qs = "SELECT " <> fst ledgerEntryFields <> " FROM tgthr.ledger WHERE id = ? "

getEntriesForJournal :: JournalId -> Connection -> IO [LedgerEntry]
getEntriesForJournal jid conn = query conn qs (Only jid)
 where
  qs =
    "SELECT "
      <> fst ledgerEntryFields
      <> [sql| FROM tgthr.ledger WHERE journal_id = ? ORDER BY revision DESC |]

unsafeDeleteLedgerTransaction
  :: LedgerTrxId -> Connection -> IO [LedgerEntryId]
unsafeDeleteLedgerTransaction tid conn = withTransaction conn $ do
  trx <- getLedgerTransaction tid conn
  case trx of
    Nothing -> return []
    Just lt -> do
      let ent = case ltxEntries lt of
            EntriesById leis -> leis
            Entries     les  -> fmap lenId les
      _ <- execute conn deletetrx (Only tid)
      mapM_ (execute conn deleteEntry . Only) ent
      return ent
 where
  deletetrx   = [sql| DELETE FROM tgthr.ledger_transaction WHERE id = ? |]
  deleteEntry = [sql| DELETE FROM tgthr.ledger WHERE id = ? |]

getLedgerEntry :: LedgerEntryId -> Connection -> IO (Maybe LedgerEntry)
getLedgerEntry lid conn = listToMaybe <$> query conn qs (Only lid)
 where
  qs =
    "SELECT "
      <> fst ledgerEntryFields
      <> [sql| FROM tgthr.ledger WHERE id = ? |]

getLedgerTransaction
  :: LedgerTrxId -> Connection -> IO (Maybe LedgerTransaction)
getLedgerTransaction tid conn = do
  trxM :: Maybe LedgerTransaction <- listToMaybe <$> query conn qs (Only tid)
  case trxM of
    Nothing -> return Nothing
    Just t  -> case ltxEntries t of
      Entries     _   -> return $ Just t
      EntriesById ids -> do
        entries <- fmap fromJust <$> mapM (`getLedgerEntry` conn) ids
        return $ Just t { ltxEntries = Entries entries }
 where
  qs =
    "SELECT "
      <> fst ledgerTransactionlFields
      <> [sql| FROM tgthr.ledger_transaction WHERE id = ? |]
