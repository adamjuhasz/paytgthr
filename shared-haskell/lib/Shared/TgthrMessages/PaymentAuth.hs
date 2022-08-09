{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables, StrictData #-}

module Shared.TgthrMessages.PaymentAuth
  ( module Shared.TgthrMessages.PaymentAuth
  , PlaidEnvironment(..)
  , PlaidAccountId(..)
  , PublicToken(..)
  ) where

import           Data.Aeson                     ( FromJSON(..)
                                                , ToJSON(..)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( CardId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                )
import           Shared.Models.Plaid.Base       ( PlaidAccountId(..)
                                                , PlaidEnvironment(..)
                                                , PublicToken(..)
                                                )
import           Shared.Models.RiskScore        ( RiskFact
                                                , RiskScore
                                                )
import           Shared.Models.Transaction      ( Transaction
                                                , TransactionId
                                                , TransactionSrcId
                                                , TransactionState
                                                )
import           Shared.TgthrMessages.Base      ( AccountType
                                                , ThroughMQ(..)
                                                )
import           Shared.Utils                   ( customAesonOptions )

type ABARouting = Text
type DDANumber = Text
type AccountName = Text

-- Commands sent to payment service --
data PaymentCmd
  -- Migrated
  -- _GetSpendableBalance
  = GetSpendableBalance
    { gbcUser :: UserID
    }
  -- Migrated
  -- _AddPlaidToken
  | AddToken
    { atcUser :: UserID
    , atcPublicToken :: PublicToken
    , atcEnvPlaid :: PlaidEnvironment
    }
  -- Migrated
  -- _GetPlaidAccountList
  | GetAccountList
    { gacUser :: UserID
    }
  -- Migrated
  -- _SetFSBankPlaid
  | SetPrimaryBank
    { sbcUser :: UserID
    , sbcAccount :: PlaidAccountId
    , sbcABARouting :: ABARouting
    , sbcDDAAccount :: DDANumber
    , sbcAccountName :: AccountName
    , sbcAccountType :: AccountType
    }
  -- Migrated
  -- _GetPayment
  | GetPayment
    { gpcPayment :: PaymentId
    }
  -- Migrated
  -- _QueryPurchaseSourceId
  | GetTransactionFromAptoId
    { gtcAptoTransactionId :: TransactionSrcId
    }
  -- Migrated
  -- __QueryPaymentSourceId
  | GetPaymentFromSourceId
    { gxcSourceId :: Text
    }
  -- Migrated
  -- _GetPurchase
  | GetTransaction
    { gtcTransaction :: TransactionId
    }
  -- Migrated
  -- _GetPaymentsCreatedAt
  | GetPaymentPendingTime
    { gycPayment :: PaymentId
    }
  -- Migrated
  -- _AdminRefreshPlaidBalances
  | RefreshAllBalances
  -- Migrated
  -- _RefreshPlaidBalance
  | RefreshBalance
    { rbcUser :: UserID
    }
  -- Migrated
  -- _GetLiability
  | GetUsersLiability
    { glcUser :: UserID
    }
  -- Migrated
  -- _GetTrustScore
  | GetUsersRiskScore
    { grcUser :: UserID
    }
  -- Migrated
  -- _UpdateTrustScore
  | AdjustUsersRiskScore
    { arcUser :: UserID
    , arcFact :: RiskFact
    }
  -- Migrated
  -- _GetPurchases
  | GetMostRecentTransactions
    { gmcUser :: UserID
    , gmcCount :: Int
    }
  -- Migrated
  -- _SendStatement
  | RequestStatement
    { rscUser :: UserID
    , rscRange :: Maybe (UTCTime, UTCTime)
    , rscLastMonths :: Maybe Int
    }
  deriving (Eq, Show, Generic)
instance ThroughMQ PaymentCmd where
  toKey GetSpendableBalance{}       = "paymentauth.cmd.getspendablebalance"
  toKey AddToken{}                  = "paymentauth.cmd.addtoken"
  toKey GetAccountList{}            = "paymentauth.cmd.getaccountlist"
  toKey SetPrimaryBank{}            = "paymentauth.cmd.setprimarybank"
  toKey GetPayment{}                = "paymentauth.cmd.getpayment"
  toKey GetTransactionFromAptoId{}  = "paymentauth.cmd.gettransactionfromapto"
  toKey GetPaymentFromSourceId{}    = "paymentauth.cmd.getpaymentfromsource"
  toKey GetTransaction{}            = "paymentauth.cmd.gettransaction"
  toKey GetPaymentPendingTime{}     = "paymentauth.cmd.getpaymentpendingtime"
  toKey RefreshAllBalances{}        = "paymentauth.cmd.refreshallbalances"
  toKey RefreshBalance{}            = "paymentauth.cmd.refreshbalance"
  toKey GetUsersLiability{}         = "paymentauth.cmd.getuserliability"
  toKey GetUsersRiskScore{}         = "paymentauth.cmd.getuserriskscore"
  toKey AdjustUsersRiskScore{}      = "paymentauth.cmd.adjustuserriskscore"
  toKey GetMostRecentTransactions{} = "paymentauth.cmd.getmostrecentrxs"
  toKey RequestStatement{}          = "paymentauth.cmd.requeststatement"

instance FromJSON PaymentCmd where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON PaymentCmd where
  toJSON = genericToJSON customAesonOptions

data PaymentEvents
  = BalanceRefreshed
    { breUser :: UserID
    , breBalance :: Currency
    }
  | UserLinked
    { uleUser :: UserID
    }
  | ManualVerificationWrong
    { meeUser :: UserID
    }
  | TransactionUpdated
    { tueUser :: UserID
    , tueTransaction :: TransactionId
    , tueStatus :: TransactionState
    , tuePrevStatus :: TransactionState
    }
  | BalanceRefreshFail
    { bfeUser :: UserID
    }
  | LedgerUpdated
    { lueUser :: UserID
    , lueBalance :: Currency
    , lueChange :: Currency
    , lueFact :: LedgerFact
    , lueRevision :: Integer
    , lueTime :: UTCTime
    }
  | RiskUpdated
    { rueUser :: UserID
    , rueTrustScore :: Double
    , rueChange :: Double
    , rueFact :: RiskFact
    , rueRevision :: Int
    , rueTime :: UTCTime
    }
  | StatementRequested
    { sreUser :: UserID
    , sreRange :: Maybe (UTCTime, UTCTime)
    , sreLastMonths :: Maybe Int
    }
  deriving (Eq, Show, Generic)
instance ThroughMQ PaymentEvents where
  toKey BalanceRefreshed{}        = "paymentauth.event.balancerefreshed"
  toKey UserLinked{}              = "paymentauth.event.userlinked"
  toKey TransactionUpdated{}      = "paymentauth.event.transactionupdated"
  toKey BalanceRefreshFail{}      = "paymentauth.event.balancerefreshfail"
  toKey ManualVerificationWrong{} = "paymentauth.event.manualverificationwrong"
  toKey LedgerUpdated{}           = "paymentauth.event.ledgerupdated"
  toKey RiskUpdated{}             = "paymentauth.event.riskupdated"
  toKey StatementRequested{}      = "paymentauth.event.statementrequested"
instance FromJSON PaymentEvents where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON PaymentEvents where
  toJSON = genericToJSON customAesonOptions

data RiskRules
  = ExceedMaxPerTrxAmount Currency
  | RiskyMerchantName
  | RiskyMerchantMCC
  | RiskyP2P
  | CombinedRiskRules
  | RiskyMerchantCountry
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type ExpectedShare = Currency
type Balance = Currency
data AuthResult
  = Success Transaction
  | UserMissing Transaction [UserID]
  | GroupNotFound Transaction
  | GroupNotActive Transaction
  | BalanceRequestFailed Transaction [UserID]
  | InsufficentFunds Transaction [(UserID, ExpectedShare, Balance)]
  | RiskTriggered Transaction RiskRules
  | NoPaymentLinked Transaction [UserID]
  | AccountClosed Transaction [UserID]
  | AccountNotActive Transaction [UserID]
  | CardNotActivated Transaction CardId
  | CardFrozen Transaction CardId
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

extractTrxFromAuthResult :: AuthResult -> Transaction
extractTrxFromAuthResult (Success t               ) = t
extractTrxFromAuthResult (UserMissing t _         ) = t
extractTrxFromAuthResult (GroupNotFound  t        ) = t
extractTrxFromAuthResult (GroupNotActive t        ) = t
extractTrxFromAuthResult (BalanceRequestFailed t _) = t
extractTrxFromAuthResult (InsufficentFunds     t _) = t
extractTrxFromAuthResult (RiskTriggered        t _) = t
extractTrxFromAuthResult (NoPaymentLinked      t _) = t
extractTrxFromAuthResult (AccountClosed        t _) = t
extractTrxFromAuthResult (AccountNotActive     t _) = t
extractTrxFromAuthResult (CardNotActivated     t _) = t
extractTrxFromAuthResult (CardFrozen           t _) = t

data AccountDetails = AccountDetails
  { acctId         :: PlaidAccountId
  , acctName       :: Text
  , acctABARouting :: ABARouting
  , acctDDNNumber  :: DDANumber
  , acctCurrBal    :: Currency
  , acctType       :: AccountType
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data PaymentReplies
  = GetBalanceReply
    { gbrUser :: UserID
    , gbrBalance :: Currency
    }
  | AccountListReply
    { alrUserid :: UserID
    , alrAccounts :: [(PlaidAccountId, AccountName, ABARouting, DDANumber, Currency)]
    }
  | AccountListReplyV2
    { a2rUser :: UserID
    , a2rAccounts :: [AccountDetails]
    }
  | GetPaymentReply
    { gprPayment :: PaymentId
    , gprPaymentModel :: Payment
    }
  | GetTransactionReply
    { gtrTransaction :: TransactionId
    , gtrTransactionModel :: Transaction
    }
  | GetPaymentPendingTimeReply
    { gyrPayment :: PaymentId
    , gyrCreatedAt :: UTCTime
    }
  | GetTransactionsReply
    { grrTransactions :: [Transaction]
    }
  | GetUsersLiabilityReply
    { glrUser :: UserID
    , glrLiability :: Currency
    }
  | GetUsersRiskScoreReply
    { grrUser :: UserID
    , grrRiskScore :: RiskScore
    }
  | GiftBoxReply
    { gbrUser :: UserID
    , gbrAmount :: Currency
    }
  deriving (Eq, Show, Generic)
instance FromJSON PaymentReplies where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON PaymentReplies where
  toJSON = genericToJSON customAesonOptions
