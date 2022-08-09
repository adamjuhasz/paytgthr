{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Shared.WebAPI.PaymentAuth.API
  ( module Shared.WebAPI.PaymentAuth.API
  , PublicToken(..)
  , AuthResult(..)
  , TraceContext(..)
  , traceToMID
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Servant
import           Servant.API.Generic            ( Generic
                                                , GenericMode(..)
                                                )
import           Shared.Models.Card             ( IssuerPlatform )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ledger.Common    ( LedgerFact )
import           Shared.Models.Ledger.Entry     ( LedgerEntry )
import           Shared.Models.Ledger.Journal   ( JournalId
                                                , JournalSearch
                                                , JournalType
                                                , LedgerJournal
                                                )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                , PaymentStatus
                                                )
import           Shared.Models.Plaid.Base       ( PublicToken(..) )
import           Shared.Models.RiskScore        ( RiskFact
                                                , RiskScore
                                                )
import           Shared.Models.Transaction      ( AptoAdjustment
                                                , MerchantInfo
                                                , Transaction
                                                , TransactionDetails
                                                , TransactionEvent
                                                , TransactionId
                                                , TransactionSource
                                                , TransactionSrcId
                                                , TransactionState
                                                )
import           Shared.Models.User             ( RedactedText
                                                , UserID
                                                , UserModel
                                                )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult(..) )
import           Shared.WebAPI.General.API      ( TraceContext(..)
                                                , TraceHeaders
                                                , traceToMID
                                                )

type ABARouting = RedactedText
type DDANumber = RedactedText

data UpdatePaymentBody = UpdatePaymentBody
  { paymentStatus      :: PaymentStatus
  , paymentSetACHInfo  :: Maybe (ABARouting, DDANumber)
  , paymentSetMethodId :: Maybe Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON UpdatePaymentBody
instance FromJSON UpdatePaymentBody

data VerificationPaymentStyle
  = UserSavedAmounts
  | UseSpecic [Double]
  deriving (Eq, Show, Generic)
instance ToJSON VerificationPaymentStyle
instance FromJSON VerificationPaymentStyle

newtype MakeVerificationPaymentBody = MakeVerificationPaymentBody
  { verificationStyle :: VerificationPaymentStyle
  }
  deriving (Eq, Show, Generic)
instance ToJSON MakeVerificationPaymentBody
instance FromJSON MakeVerificationPaymentBody

newtype GetPlaidLinkTokenResponse = GetPlaidLinkTokenResponse
  { linkToken :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON GetPlaidLinkTokenResponse
instance FromJSON GetPlaidLinkTokenResponse

data AuthorizePurchaseBody = AuthorizePurchaseBody
  { transactionId     :: TransactionId
  , source            :: TransactionSource
  , sourceId          :: Text
  , sourceIdempotency :: Maybe Text
  , amount            :: Currency
  , description       :: Text
  , merchant          :: Maybe MerchantInfo
  , details           :: Maybe TransactionDetails
  , cardUsed          :: IssuerPlatform
  }
  deriving (Eq, Show, Generic)
instance ToJSON AuthorizePurchaseBody
instance FromJSON AuthorizePurchaseBody

data SetManualFSBody = SetManualFSBody
  { unverifiedABARouting  :: ABARouting
  , unverifiedDDAAccount  :: DDANumber
  , unverifiedAccountName :: Text
  , unverifiedBankName    :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON SetManualFSBody
instance FromJSON SetManualFSBody

newtype VerifyFSBankBody = VerifyFSBankBody
  { userEnteredAmounts :: [Double]
  }
  deriving (Eq, Show, Generic)
instance ToJSON VerifyFSBankBody
instance FromJSON VerifyFSBankBody

newtype VerifyFSBankResponse = VerifyFSBankResponse
  { verificationSuccess :: Bool
  }
  deriving (Eq, Show, Generic)
instance ToJSON VerifyFSBankResponse
instance FromJSON VerifyFSBankResponse

data UpdatePurchaseBody = UpdatePurchaseBody
  { purchaser        :: UserID
  , transactionId    :: TransactionId
  , source           :: TransactionSource
  , idempotency      :: Maybe Text
  , sourceId         :: TransactionSrcId
  , cardId           :: IssuerPlatform
  , details          :: TransactionDetails
  , merchant         :: MerchantInfo
  , standin          :: Bool
  , state            :: TransactionState
  , createdAt        :: UTCTime
  , transactionEvent :: TransactionEvent
  , amountLocal      :: Currency
  , amountHold       :: Currency
  , amountCashback   :: Currency
  , amountFee        :: Currency
  , amountBilling    :: Currency
  , description      :: Maybe Text
  , adjustments      :: [AptoAdjustment]
  }
  deriving (Eq, Show, Generic)
instance ToJSON UpdatePurchaseBody
instance FromJSON UpdatePurchaseBody

newtype AdjustTrustScoreBody = AdjustTrustScoreBody
  { newFact :: RiskFact
  }
  deriving (Eq, Show, Generic)
instance ToJSON AdjustTrustScoreBody
instance FromJSON AdjustTrustScoreBody

data SetPlaidAccountBody = SetPlaidAccountBody
  { publicToken :: PublicToken
  , accountId   :: Text
  }
  deriving (Eq, Show, Generic)
instance ToJSON SetPlaidAccountBody
instance FromJSON SetPlaidAccountBody

newtype GetLedgerResponse = GetLedgerResponse
  { ledger :: LedgerEntry
  }
  deriving (Eq, Show, Generic)
instance ToJSON GetLedgerResponse
instance FromJSON GetLedgerResponse

data CreateLedgerJournalBody = CreateLedgerJournalBody
  { newJournalId   :: JournalId
  , newJournalType :: JournalType
  , newJournalName :: Text
  , startBalance   :: Currency
  }
  deriving (Eq, Show, Generic)
instance ToJSON CreateLedgerJournalBody
instance FromJSON CreateLedgerJournalBody

data CreateLedgerTrxBody = CreateLedgerTrxBody
  { fromJournal :: JournalId
  , toJournal   :: JournalId
  , fact        :: LedgerFact
  }
  deriving (Eq, Show, Generic)
instance ToJSON CreateLedgerTrxBody
instance FromJSON CreateLedgerTrxBody

-- inline brittany config for width
-- brittany-next-binding --columns 500
data Routes route = Routes
  -- User
  { _GetSpendableBalance       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "balance" :> "spendable" :> Get '[JSON] Currency
  , _MakeVerificationPayment   :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "payment" :> "verification" :> ReqBody '[JSON] MakeVerificationPaymentBody :> Post '[JSON] NoContent
  , _GetPlaidLinkToken         :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "plaid" :> "linktoken" :> Get '[JSON] GetPlaidLinkTokenResponse
  , _SetPlaidAccount           :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "plaid" :> "account" :> ReqBody '[JSON] SetPlaidAccountBody :> Post '[JSON] NoContent
  -- , _RefreshPlaidBalance       :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "plaid" :> "balance" :> Post '[JSON] NoContent
  , _AuthorizeCardPurchase     :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "authorize" :> "purchase" :> "debitcard" :> ReqBody '[JSON] AuthorizePurchaseBody :> Post '[JSON] AuthResult
  , _SetFSBankManual           :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> "bank" :> "manual" :> ReqBody '[JSON] SetManualFSBody :> Post '[JSON] NoContent
  , _VerifyBankAccount         :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "fundingsource" :> "bank" :> "verify" :> ReqBody '[JSON] VerifyFSBankBody :> Post '[JSON] VerifyFSBankResponse
  , _GetLiability              :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "liability" :> Get '[JSON] Currency
  , _GetTrustScore             :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "trustscore" :> Get '[JSON] RiskScore
  -- , _UpdateTrustScore          :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "trustscore" :> ReqBody '[JSON] AdjustTrustScoreBody :> Put '[JSON] NoContent
  , _GetPurchases              :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "purchases" :> QueryParam' '[Required] "count" Int :> Get '[JSON] [Transaction]
  , _GetPayments               :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "payments" :> QueryParams "state" PaymentStatus :> Get '[JSON] [Payment]
  -- , _SendStatement             :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "statement" :> Post '[JSON] NoContent
  , _PayoutLedger              :: route :- TraceHeaders :> "user" :> Capture "userid" UserID :> "ledger" :> "payout" :> Post '[JSON] NoContent

  -- Payment
  , _GetPayment                :: route :- TraceHeaders :> "payment" :> Capture "paymentid" PaymentId :> Get '[JSON] Payment
  , _UpdatePayment             :: route :- TraceHeaders :> "payment" :> Capture "paymentid" PaymentId :> ReqBody '[JSON] UpdatePaymentBody :> Put '[JSON] NoContent
  -- , _GetPaymentsCreatedAt      :: route :- TraceHeaders :> "payment" :> Capture "paymentid" PaymentId :> "createdat" :> Get '[JSON] Payment
  , _QueryPaymentSourceId      :: route :- TraceHeaders :> "payment" :> "query" :> "sourceid" :> Capture "SourceId" Text :> Get '[JSON] Payment
  , _CancelPayment             :: route :- TraceHeaders :> "payment" :> Capture "paymentid" PaymentId :> "cancel" :> Put '[JSON] NoContent
  , _GetUsersNeedToRepayUs     :: route :- TraceHeaders :> "payments" :> "repaymentNeeded" :> Get '[JSON] [(UserModel, Double)]

  -- Purchase
  , _GetPurchase               :: route :- TraceHeaders :> "purchase" :> Capture "transactonId" TransactionId :> Get '[JSON] Transaction
  , _UpdatePurchase            :: route :- TraceHeaders :> "purchase" :> Capture "transactonId" TransactionId :> ReqBody '[JSON] UpdatePurchaseBody :> Put '[JSON] NoContent
  , _QueryPurchaseSourceId     :: route :- TraceHeaders :> "purchase" :> "query" :> "sourceid" :> Capture "TransactionSrcId" TransactionSrcId :> Get '[JSON] (Maybe Transaction)
  , _SetPurchaseStateDirectly  :: route :- TraceHeaders :> "purchase" :> Capture "transactonId" TransactionId :> "state" :> ReqBody '[JSON] TransactionState :> Put '[JSON] NoContent

  -- Ledger
  , _CreateJournal             :: route :- TraceHeaders :> "ledger" :> "journal" :> ReqBody '[JSON] CreateLedgerJournalBody :> Post '[JSON] NoContent
  , _CreateJournalTrx          :: route :- TraceHeaders :> "ledger" :> "transaction" :> ReqBody '[JSON] CreateLedgerTrxBody :> Post '[JSON] NoContent
  , _GetJournal                :: route :- TraceHeaders :> "ledger" :> "journal" :> "search" :>  ReqBody '[JSON] JournalSearch :> Post '[JSON] [LedgerJournal]
  , _GetEntries                :: route :- TraceHeaders :> "ledger" :> "journal" :> Capture "JournalId" JournalId :> Get '[JSON] [LedgerEntry]

  -- Admin
  , _AdminLedgerToPayments     :: route :- TraceHeaders :> "admin" :> "ledger" :> "process" :> Post '[JSON] NoContent
  , _AdminRefreshPlaidBalances :: route :- TraceHeaders :> "admin" :> "plaid" :> "refresh" :> "all" :> Post '[JSON] NoContent

  -- System
  , _health                    :: route :- "_health" :> Get '[PlainText] Text
  }
  deriving Generic
