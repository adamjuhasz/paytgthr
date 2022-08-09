{-# LANGUAGE QuasiQuotes #-}

module PaymentAuth.DB.Transaction where

import           Data.Maybe                     ( listToMaybe )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(Only)
                                                , execute
                                                , query
                                                , query_
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.PaymentAuth      ( UserID )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionId(..)
                                                , TransactionSrcId
                                                , sqlTransactionSelect
                                                )

saveTransaction :: Transaction -> Connection -> IO ()
saveTransaction trx conn = withTransaction conn $ do
  _ <- execute conn insert trx
  _ <- execute conn upsert trx
  return ()
 where
  insert =
    "INSERT INTO tgthr.transactions ( "
      <> fst sqlTransactionSelect
      <> ") VALUES ( "
      <> snd sqlTransactionSelect
      <> ")"
  upsert =
    "UPSERT INTO tgthr.transactions_current_rev ( "
      <> fst sqlTransactionSelect
      <> ") VALUES ( "
      <> snd sqlTransactionSelect
      <> ")"

loadTransaction :: TransactionId -> Connection -> IO (Maybe Transaction)
loadTransaction tid conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst sqlTransactionSelect
      <> " FROM tgthr.transactions_current_rev WHERE id = ? "
  selector = Only tid

loadTransactionFromAptoId
  :: TransactionSrcId -> Connection -> IO (Maybe Transaction)
loadTransactionFromAptoId tid conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst sqlTransactionSelect
      <> " FROM tgthr.transactions_current_rev WHERE source_id = ? "
  selector = Only tid

getPendingTransactions :: UserID -> Connection -> IO [Transaction]
getPendingTransactions uid conn = query conn qs selector
 where
  qs =
    "SELECT "
      <> fst sqlTransactionSelect
      <> " FROM tgthr.transactions_current_rev WHERE state = 'pending' AND (split_amounts->0->>0 = ? OR split_amounts->1->>0 = ?)"
  selector = (uid, uid)

getAllPendingTransactions :: Connection -> IO [Transaction]
getAllPendingTransactions conn = query_ conn qs
 where
  qs =
    "SELECT "
      <> fst sqlTransactionSelect
      <> " FROM tgthr.transactions_current_rev WHERE state = 'pending'"

getUsersTransactions :: UserID -> Int -> Connection -> IO [Transaction]
getUsersTransactions uid limit conn = query
  conn
  ("SELECT " <> fst sqlTransactionSelect <> [sql|  
    FROM (
      SELECT * 
      FROM tgthr.transactions_current_rev 
      WHERE id IN (
        SELECT id 
        FROM tgthr.transactions_byuserid 
        WHERE user_id = ?
      ) 
    ) 
    ORDER BY purchased_at DESC LIMIT ? 
  |]
  )
  (uid, limit)
