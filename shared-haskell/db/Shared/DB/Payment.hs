{-# LANGUAGE QuasiQuotes #-}

module Shared.DB.Payment where

import           Data.Maybe                     ( listToMaybe )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(Only)
                                                , execute
                                                , query
                                                , query_
                                                , withTransaction
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids              ( PaymentId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Payment          ( Payment
                                                , sqlPaymentSelect
                                                )

savePayment :: Payment -> Connection -> IO ()
savePayment payment conn = withTransaction conn $ do
  _ <- execute conn insert payment
  _ <- execute conn upsert payment
  return ()
 where
  insert =
    "INSERT INTO tgthr.payments ( "
      <> fst sqlPaymentSelect
      <> ") VALUES ( "
      <> snd sqlPaymentSelect
      <> ")"
  upsert =
    "UPSERT INTO tgthr.payments_current_rev ( "
      <> fst sqlPaymentSelect
      <> ") VALUES ( "
      <> snd sqlPaymentSelect
      <> ")"

loadPayment :: PaymentId -> Connection -> IO (Maybe Payment)
loadPayment pid conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst sqlPaymentSelect
      <> " FROM tgthr.payments_current_rev WHERE id = ? "
  selector = Only pid

getPaymentFromSourceId :: Text -> Connection -> IO (Maybe Payment)
getPaymentFromSourceId anId conn = listToMaybe <$> query conn qs selector
 where
  qs =
    "SELECT "
      <> fst sqlPaymentSelect
      <> " FROM tgthr.payments_current_rev WHERE pay_method_id = ? "
  selector = Only anId

getUsersPendingPayments :: UserID -> Connection -> IO [Payment]
getUsersPendingPayments (UserID uid) conn = query conn qs selector
 where
  qs =
    "SELECT"
      <> fst sqlPaymentSelect
      <> "FROM tgthr.payments_current_rev WHERE user_id = ? AND status IN ( 'pending', 'created')"
  selector = Only uid

getPendingPaymentCreatedAt :: PaymentId -> Connection -> IO (Maybe UTCTime)
getPendingPaymentCreatedAt (PaymentId pid) conn =
  fmap (\(Only cat) -> cat) . listToMaybe <$> query conn qs selector
 where
  qs
    = "SELECT created_at FROM tgthr.payments WHERE id = ? AND status IN ( 'pending', 'created')"
  selector = Only pid

getPaymentsForUser :: UserID -> Connection -> IO [Payment]
getPaymentsForUser uid conn = query conn qs $ Only uid
 where
  qs =
    "SELECT "
      <> fst sqlPaymentSelect
      <> " FROM tgthr.payments_current_rev WHERE user_id = ? ORDER BY created_at DESC"

getPaymentRevisions :: PaymentId -> Connection -> IO [Payment]
getPaymentRevisions pid conn = query conn qs $ Only pid
 where
  qs =
    "SELECT "
      <> fst sqlPaymentSelect
      <> [sql| FROM tgthr.payments WHERE id = ? ORDER BY revision DESC |]

getUsersPaymentRevs :: UserID -> Connection -> IO [Payment]
getUsersPaymentRevs uid conn = query conn paymentQs $ Only uid
 where
  paymentQs =
    "SELECT "
      <> fst sqlPaymentSelect
      <> [sql| FROM tgthr.payments_current_rev WHERE user_id = ? ORDER BY created_at DESC |]

getAllPayments :: Connection -> IO [Payment]
getAllPayments conn =
  query_ conn
    $  "SELECT "
    <> fst sqlPaymentSelect
    <> [sql| FROM tgthr.payments_current_rev ORDER BY created_at DESC |]
