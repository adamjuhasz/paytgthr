{-# LANGUAGE StrictData #-}

module PaymentAuth.Types where

import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.Ids              ( MessageID(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                )
import           Shared.Models.PaymentAuth      ( AccessToken
                                                , Account
                                                , Currency
                                                , ItemId
                                                , PlaidAccountId
                                                , PlaidTokenRow
                                                , UserID
                                                )
import           Shared.Models.Plaid.Base       ( PlaidEnvironment )
import           Shared.Models.RiskScore        ( RiskScore )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionId
                                                , TransactionSrcId
                                                )

newtype CurrentBalance = CurrentBalance Currency deriving (Eq, Show, Ord)

data DBActions = DBActions
  -- HasPlaidDB
  { dbInsertBalance       :: MessageID -> UserID -> (Account, Double) -> IO ()
  , dbGetTokens           :: IO [(AccessToken, PlaidEnvironment)]
  -- HasPlaidDB
  , dbGetPrimaryAccount   :: UserID -> IO (Maybe PlaidAccountId)
  -- HasPlaidDB
  , dbGetAccessToken :: UserID -> IO (Maybe (AccessToken, PlaidEnvironment))
  , dbGetPlaidTokenRow    :: UserID -> IO (Maybe PlaidTokenRow)
  , dbInsertPlaidTokenRow :: PlaidTokenRow -> IO ()
  -- HasPlaidDB
  , dbUpdateTokenPrimary
      :: MessageID -> UserID -> (PlaidAccountId, Text, Text) -> IO ()
  -- HasPlaidDB
  , dbInsertToken
      :: MessageID -> UserID -> (AccessToken, ItemId, PlaidEnvironment) -> IO ()
  -- HasPlaidDB
  , dbGetRecentBalanceSince
      :: UserID -> UTCTime -> IO (Maybe (Rational, UTCTime))
  , dbGetUserFromItem            :: ItemId -> IO (Maybe UserID)
  -- HasTransactionsDB
  , dbSaveTransaction            :: Transaction -> IO ()
  -- HasTransactionsDB
  , dbLoadTransaction            :: TransactionId -> IO (Maybe Transaction)
  -- HasTransactionsDB
  , dbLoadTransactionFromAptoId  :: TransactionSrcId -> IO (Maybe Transaction)
  -- HasPaymentsDB
  , dbSavePayment                :: Payment -> IO ()
  -- HasPaymentsDB
  , dbLoadPayment                :: PaymentId -> IO (Maybe Payment)
  -- HasPaymentsDB
  , dbGetPaymentFromSourceId     :: Text -> IO (Maybe Payment)
  -- HasPaymentsDB
  , dbGetUsersPendingPayments    :: UserID -> IO [Payment]
  -- HasPaymentsDB
  , dbGetPendingPaymentCreatedAt :: PaymentId -> IO (Maybe UTCTime)
  -- HasTransactionsDB
  , dbGetPendingTransactions     :: UserID -> IO [Transaction]
  -- HasLedgerDB
  , dbSaveLedgerEntry            :: LedgerEntry -> IO ()
  -- HasLedgerDB
  , dbGetUsersWithBalances       :: IO [(UserID, Currency)]
  , dbGetAllPendingTransactions  :: IO [Transaction]
  -- HasRiskScoresDB
  , dbGetUsersRisk               :: MessageID -> UserID -> IO RiskScore
  -- HasRiskScoresDB
  , dbSaveRiskScore              :: RiskScore -> IO ()
  -- HasTransactionsDB 
  , dbGetUsersTransactions       :: UserID -> Int -> IO [Transaction]
  }
