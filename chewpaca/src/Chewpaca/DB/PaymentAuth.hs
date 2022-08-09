{-# LANGUAGE NamedFieldPuns, QuasiQuotes #-}

module Chewpaca.DB.PaymentAuth where

import           Data.List                      ( intersperse )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Database.PostgreSQL.Simple     ( Connection
                                                , In(In)
                                                , Only(Only)
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids              ( TransactionId
                                                , UserID
                                                )
import           Shared.Models.PaymentAuth      ( PlaidBalanceRow
                                                , PlaidTokenRow
                                                , plaidBalanceRowFieldOrder
                                                , plaidTokenRowFieldOrder
                                                )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionState
                                                , sqlTransactionSelect
                                                )

getTokensForUser :: UUID -> Connection -> IO [PlaidTokenRow]
getTokensForUser uid conn = query
  conn
  (  "SELECT "
  <> fst plaidTokenRowFieldOrder
  <> " FROM tgthr.plaidtokens WHERE user_id = ? ORDER BY revision DESC"
  )
  (Only uid)

getBalancesForUser :: UUID -> Connection -> IO [(PlaidBalanceRow, UTCTime)]
getBalancesForUser uid conn = do
  balances <- query conn
                    ("SELECT " <> fst plaidBalanceRowFieldOrder <> specifier)
                    selector
  times <-
    fmap (\(Only x) -> x)
      <$> query conn ("SELECT created_at " <> specifier) selector
  return $ zip balances times
 where
  specifier = " FROM tgthr.balances WHERE user_id = ? ORDER BY created_at DESC"
  selector  = Only uid

getUsersTransactions :: UUID -> Connection -> IO [Transaction]
getUsersTransactions uid conn = query
  conn
  ("SELECT " <> fst sqlTransactionSelect <> [sql| 
    FROM tgthr.transactions_current_rev 
    WHERE splitting_users @> ARRAY[?] 
    ORDER BY purchased_at DESC;
  |]
  )
  (Only uid)

getTransactions
  :: Maybe UTCTime -> Maybe [TransactionState] -> Connection -> IO [Transaction]
getTransactions start states conn = query
  conn
  (  "SELECT "
  <> fst sqlTransactionSelect
  <> " FROM tgthr.transactions_current_rev "
  <> " WHERE "
  <> mconcat
       (intersperse
         " AND "
         [ " purchased_at >= ? "
         , case states of
           Nothing -> " state NOT IN ? "
           Just _  -> " state IN ? "
         ]
       )
  <> " ORDER BY purchased_at DESC "
  )
  (start, In $ fromMaybe [] states)

getAllTransactions :: Connection -> IO [(Transaction, UTCTime)]
getAllTransactions conn = do
  trxs <- query_
    conn
    (  "SELECT "
    <> fst sqlTransactionSelect
    <> " FROM tgthr.transactions_current_rev ORDER BY purchased_at DESC "
    )
  times <- fmap (\(Only x) -> x) <$> query_
    conn
    "SELECT created_at FROM tgthr.transactions_current_rev ORDER BY purchased_at DESC "
  return $ zip trxs times

getMatchTestTransactions :: Connection -> IO [Transaction]
getMatchTestTransactions conn = query_ conn qs
 where
  qs = "SELECT " <> fst sqlTransactionSelect <> [sql|
    FROM tgthr.transactions_current_rev
    WHERE
    (
      (
        state = 'completed'
        OR state = 'pending'
      )
      AND purchased_at >= TIMESTAMPTZ '2021-01-01 00:00:00.000-08:00'
      AND (
          amount_number != 0
          OR amount_number IS NULL
        )
    )
    ORDER BY
      purchased_at DESC 
    |]

getSingleTransactionRevs
  :: TransactionId -> Connection -> IO [(Transaction, UTCTime)]
getSingleTransactionRevs tid conn = do
  trxs  <- query conn qs selector
  times <-
    fmap (\(Only x) -> x) <$> query conn ("SELECT created_at " <> from) selector
  return $ zip trxs times
 where
  selector = Only tid
  from     = " FROM tgthr.transactions WHERE id = ? ORDER BY revision DESC"
  qs       = "SELECT " <> fst sqlTransactionSelect <> from

getGlobalPurchasesInLast :: Text -> Connection -> IO [Transaction]
getGlobalPurchasesInLast interval conn = query conn qs (Only interval)
 where
  qs = "SELECT " <> fst sqlTransactionSelect <> [sql| 
      FROM tgthr.transactions_current_rev 
      WHERE purchased_at >= now() - interval ? 
      ORDER BY purchased_at DESC 
      |]

getUserPurchasesInLast :: UserID -> Text -> Connection -> IO [Transaction]
getUserPurchasesInLast uid interval conn = query conn qs (interval, uid)
 where
  qs = "SELECT " <> fst sqlTransactionSelect <> [sql| 
      FROM tgthr.transactions_current_rev 
      WHERE id IN (
          SELECT id 
          FROM tgthr.transactions_byuserid 
          WHERE purchased_at >= now() - interval ?
            AND user_id = ?
      ) 
      ORDER BY purchased_at DESC
      |]
