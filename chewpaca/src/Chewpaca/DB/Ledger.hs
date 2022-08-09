{-# LANGUAGE QuasiQuotes #-}

module Chewpaca.DB.Ledger where

import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(Only)
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids              ( JournalId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..)
                                                , ledgerEntryFields
                                                )
import           Shared.Models.Ledger.Journal   ( JournalType(..)
                                                , LedgerJournal
                                                , journalTag
                                                , ledgerJournalFields
                                                )

getLedgerForUser :: UUID -> Connection -> IO [(LedgerEntry, UTCTime)]
getLedgerForUser uid conn = do
  ledger <- query conn qs selector
  times  <-
    fmap (\(Only x) -> x)
      <$> query conn ("SELECT created_at " <> specifier) selector
  return $ zip ledger times
 where
  qs        = "SELECT " <> fst ledgerEntryFields <> specifier
  specifier = " FROM tgthr.ledger WHERE user_id = ? ORDER BY created_at DESC "
  selector  = Only uid

getAllEntriesForUser :: UserID -> Connection -> IO [LedgerEntry]
getAllEntriesForUser uid conn = query conn qs selector
 where
  qs =
    "SELECT "
      <> fst ledgerEntryFields
      <> " FROM tgthr.ledger WHERE user_id = ? ORDER BY created_at DESC "
  selector = Only uid

getEntireLedger :: Connection -> IO [LedgerEntry]
getEntireLedger conn = query_
  conn
  (  [sql| SELECT |]
  <> fst ledgerEntryFields
  <> [sql| FROM ledger_current_rev ORDER BY id ASC |]
  )

getAllJournalsOf :: JournalType -> Connection -> IO [LedgerJournal]
getAllJournalsOf jType conn = query conn qs (Only $ journalTag jType)
 where
  qs =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE kind = ? ORDER BY updated_at DESC"

getUserJournals :: UserID -> Connection -> IO [LedgerJournal]
getUserJournals uid conn = query conn qs (Only uid)
 where
  qs =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE user_id = ? ORDER BY updated_at DESC"

getEntriesForJournal :: JournalId -> Connection -> IO [LedgerEntry]
getEntriesForJournal jid conn = query conn qs (Only jid)
 where
  qs = "SELECT " <> fst ledgerEntryFields <> [sql| 
      FROM tgthr.ledger 
      WHERE journal_id = ? 
      ORDER BY created_at DESC
      |]

getNewRewardJournals :: Text -> Connection -> IO [LedgerEntry]
getNewRewardJournals interval conn = query conn qs (interval, interval)
 where
  qs = "SELECT " <> fst ledgerEntryFields <> [sql|
        FROM
          tgthr.ledger
        WHERE
          type != 'initialBalance'
          AND journal_id
            IN (
                SELECT
                  id
                FROM
                  tgthr.journals_current_rev
                WHERE
                  updated_at >= now() - interval ?
                  AND kind = 'PayTgthrRewards'
                )
          AND created_at >= now() - interval ?
  |]

getNewRewardJournalsForUser :: UserID -> Text -> Connection -> IO [LedgerEntry]
getNewRewardJournalsForUser uid interval conn = query
  conn
  qs
  (interval, uid, interval)
 where
  qs = "SELECT " <> fst ledgerEntryFields <> [sql|
        FROM
          tgthr.ledger
        WHERE
          type != 'initialBalance'
          AND journal_id
            IN (
                SELECT
                  id
                FROM
                  tgthr.journals_current_rev
                WHERE
                  updated_at >= now() - interval ?
                  AND kind = 'PayTgthrRewards'
                  AND user_id = ?
                )
          AND created_at >= now() - interval ?
  |]

getJournal :: JournalId -> Connection -> IO (Maybe LedgerJournal)
getJournal jid conn = listToMaybe <$> query conn qs (Only jid)
 where
  qs =
    "SELECT "
      <> fst ledgerJournalFields
      <> " FROM tgthr.journals_current_rev WHERE id = ?"
