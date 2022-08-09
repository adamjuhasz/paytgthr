{- HLINT ignore "Use let" -}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE StrictData, DeriveGeneric, DeriveAnyClass, RecordWildCards, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Models.Transaction
  ( module Shared.Models.Transaction
  , TransactionId(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(String)
                                                , genericParseJSON
                                                , genericToJSON
                                                , withText
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID.V4                   ( nextRandom )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( Action(Escape)
                                                , ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Base             ( Revision )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids              ( GroupId(..)
                                                , MessageID(..)
                                                , RewardId
                                                , TransactionId(..)
                                                , UserID(..)
                                                )
import           Shared.Utils                   ( customAesonOptions
                                                , stringToTime
                                                )
import           Text.Read                      ( readMaybe )

exampleTransaction :: IO Transaction
exampleTransaction = do
  tid        <- nextRandom <&> TransactionId
  mid        <- nextRandom <&> MessageID
  uid        <- nextRandom <&> UserID
  uidPartner <- nextRandom <&> UserID
  gid        <- nextRandom <&> GroupId

  return Transaction { trxId                = tid
                     , trxRevision          = 2
                     , trxVersion           = "1.0"
                     , trxMsgSource         = mid
                     , trxState             = TrxAuthorized
                     , trxSource            = Apto
                     , trxSourceId          = "trx_1234"
                     , trxSourceEvent       = AuthRequest
                     , trxUserId            = uid
                     , trxDisplayAmount     = Currency "USD" 100.01
                     , trxBillingAmounts    = []
                     , trxDetails           = Nothing
                     , trxGroupId           = Just (gid, 1)
                     , trxSourceIdempotency = Just "idem_987"
                     , trxSplitAmounts      = [(uid, 30), (uidPartner, 70)]
                     , trxMerchant          = Nothing
                     , trxDescription       = Just "Target Inc"
                     , trxPurchasedAt = stringToTime "2019-10-04T21:35:11+00:00"
                     , trxAdjustments       = []
                     , trxRewardId          = Nothing
                     }

data Transaction = Transaction
  { trxId                :: TransactionId
  , trxRevision          :: Revision
  , trxVersion           :: Text
  , trxMsgSource         :: MessageID
  , trxState             :: TransactionState
  , trxSource            :: TransactionSource
  , trxSourceId          :: TransactionSrcId
  , trxSourceEvent       :: TransactionEvent
  , trxUserId            :: UserID
  , trxDisplayAmount     :: Currency
  , trxBillingAmounts    :: [(Currency, UTCTime)]
  , trxPurchasedAt       :: UTCTime
  -- Optional
  , trxDetails           :: Maybe TransactionDetails
  , trxGroupId           :: Maybe (GroupId, Revision)
  , trxSourceIdempotency :: Maybe SourceIdempotentKey
  , trxSplitAmounts      :: [(UserID, Rational)]
  , trxMerchant          :: Maybe MerchantInfo
  , trxDescription       :: Maybe Text
  , trxAdjustments       :: [AptoAdjustment]
  , trxRewardId          :: Maybe RewardId
  }
  deriving (Eq, Show, Generic)
instance FromJSON Transaction where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON Transaction where
  toJSON = genericToJSON customAesonOptions

type TransactionSrcId = Text
type SourceIdempotentKey = Text
type FairShare = Rational

data TransactionSource
  = Apto
  | PayWithPrivacy
  | UnknownSource
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToField TransactionSource where
  toField Apto           = Escape "Apto"
  toField PayWithPrivacy = Escape "PayWithPrivacy"
  toField UnknownSource  = Escape "UnknownSource"
instance FromField TransactionSource where
  fromField a dat = fromField a dat <&> \(t :: String) -> case t of
    "Apto"           -> Apto
    "UnknownSource"  -> UnknownSource
    "PayWithPrivacy" -> PayWithPrivacy
    v                -> error $ "Source \"" <> v <> "\" unknown"

data CardNetwork
  = Visa
  | Mastercard
  | UnknownNetwork Text
  deriving (Eq, Show)
instance FromJSON CardNetwork where
  parseJSON = withText "CardNetwork" $ \s -> return $ case s of
    "VISA"       -> Visa
    "MASTERCARD" -> Mastercard
    n            -> UnknownNetwork n
instance ToJSON CardNetwork where
  toJSON Visa               = String "VISA"
  toJSON Mastercard         = String "MASTERCARD"
  toJSON (UnknownNetwork n) = String n
instance ToField CardNetwork where
  toField Visa               = Escape "VISA"
  toField Mastercard         = Escape "MASTERCARD"
  toField (UnknownNetwork u) = Escape $ TE.encodeUtf8 u
instance FromField CardNetwork where
  fromField a dat = fromField a dat <&> \t -> case t of
    "VISA"       -> Visa
    "MASTERCARD" -> Mastercard
    u            -> UnknownNetwork u

data TransactionDetails = CardTransaction
  { pcpContext         :: Text
  , pcpIsCardPresent   :: Bool
  , pcpIsOnline        :: Bool
  , pcpIsInternational :: Bool
  , pcpNetwork         :: CardNetwork
  , pcpIsEMV           :: Bool
  , pcpType            :: Maybe PurchaseType
  , pcpDescription     :: Maybe Text
  }
  deriving (Eq, Show, Generic)
instance FromJSON TransactionDetails where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON TransactionDetails where
  toJSON = genericToJSON customAesonOptions

newtype MastercardMCC = MastercardMCC Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
cardMCCToA :: Read a => MastercardMCC -> Maybe a
cardMCCToA (MastercardMCC t) = readMaybe . T.unpack $ t

data MerchantInfo = CardMerchant
  { cmiMcc      :: MastercardMCC
  , cmiMccDesc  :: Text
  , cmiName     :: Text
  , cmiLocality :: Maybe Text -- Phone? 
  , cmiRegion   :: Maybe Text   -- Sate ("CA")
  , cmiCountry  :: Text        -- Country ("USA")
  }
  deriving (Eq, Show, Generic)
instance FromJSON MerchantInfo where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON MerchantInfo where
  toJSON = genericToJSON customAesonOptions

newtype DisputeId = DisputeId Text deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
data CardDispute
  = Disputing DisputeId UTCTime
  | DisputeWon DisputeId UTCTime
  | DisputeLost DisputeId UTCTime
  deriving (Eq, Show, Read, Generic)
instance FromJSON CardDispute where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON CardDispute where
  toJSON = genericToJSON customAesonOptions

data PurchaseType
  = Signature
  | Pin
  deriving (Eq, Show, Generic)
instance FromJSON PurchaseType where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON PurchaseType where
  toJSON = genericToJSON customAesonOptions

type AccountBalance = Currency
type ShareAmount = Currency
data DeclineReason
  = LowBalance [UserID]         -- Generated by tgthr
  | BalanceCheckFail [UserID]   -- Generated by tgthr
  | ExceedMaxTrxAmount Currency -- Generated by tgthr
  | InvalidMerchant
  | LostorStolenCard
  | InvalidAmount
  | InvalidCardNumber
  | CardExpired
  | SuspectFraud                -- Generated by tgthr
  | InternationalNotAllowed
  | PinRetriesExceeded
  | IncorrectCVV
  | CardNotActivated
  | CardInactive
  | CardClosed
  | IncorrectAddress
  | IncorrectPin
  | GroupError                  -- Generated by tgthr
  | UserNotFound [UserID]       -- Generated by tgthr
  | PaymentUnlinked [UserID]    -- Generated by tgthr
  | P2PNotAllowed               -- Generated by tgthr
  | RiskyMerchant               -- Generated by tgthr
  | UserNotActive [UserID]      -- Generated by tgthr
  | Unknown Text
  deriving(Eq, Show, Read, Generic)
instance FromJSON DeclineReason where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON DeclineReason where
  toJSON = genericToJSON customAesonOptions

data TransactionState
  = TrxCreated
  | TrxAuthorized
  | TrxPending
  | TrxPendingReversal
  | TrxDeclined DeclineReason
  | TrxCompleted
  | TrxDisputed CardDispute
  deriving (Eq, Show, Read, Generic)
instance FromJSON TransactionState where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON TransactionState where
  toJSON = genericToJSON customAesonOptions

data TransactionEvent
  = AuthRequest
  | StateTransition
  | Reversal
  | Refund
  | NetworkCredit
  | NetworkDebit
  | NonFinancialEvent
  | BalanceInquiry
  | UnknownTransactionEvent
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToField TransactionEvent where
  toField AuthRequest             = Escape "authRequest"
  toField StateTransition         = Escape "stateTransition"
  toField Reversal                = Escape "reversal"
  toField Refund                  = Escape "refund"
  toField NetworkCredit           = Escape "networkCredit"
  toField NetworkDebit            = Escape "networkDebit"
  toField NonFinancialEvent       = Escape "nonFinancialEvent"
  toField BalanceInquiry          = Escape "balanceInquiry"
  toField UnknownTransactionEvent = Escape "unknown"
instance FromField TransactionEvent where
  fromField a dat = fromField a dat <&> \t -> case t of
    "authRequest"       -> AuthRequest
    "stateTransition"   -> StateTransition
    "reversal"          -> Reversal
    "refund"            -> Refund
    "networkCredit"     -> NetworkCredit
    "networkDebit"      -> NetworkDebit
    "nonFinancialEvent" -> NonFinancialEvent
    "balanceInquiry"    -> BalanceInquiry
    "unknown"           -> UnknownTransactionEvent
    u -> error $ "Unknown TransactionEvent with value \"" <> u <> "\""

instance ToField  [(Currency, UTCTime)] where
  toField = toJSONField
instance FromField  [(Currency, UTCTime)] where
  fromField = fromJSONField
instance ToField TransactionDetails where
  toField = toJSONField
instance FromField TransactionDetails where
  fromField = fromJSONField
instance ToField [(UserID, Rational)] where
  toField = toJSONField
instance FromField [(UserID, Rational)] where
  fromField = fromJSONField
instance ToField MerchantInfo where
  toField = toJSONField
instance FromField MerchantInfo where
  fromField = fromJSONField
instance ToField CardDispute where
  toField = toJSONField
instance FromField CardDispute where
  fromField = fromJSONField
instance ToField DeclineReason where
  toField = toJSONField
instance FromField DeclineReason where
  fromField = fromJSONField
instance ToField [AptoAdjustment] where
  toField = toJSONField
instance FromField [AptoAdjustment] where
  fromField = fromJSONField

instance ToField TransactionState where
  toField TrxCreated         = Escape "created"
  toField TrxAuthorized      = Escape "authorized"
  toField TrxPending         = Escape "pending"
  toField TrxPendingReversal = Escape "pendingReversal"
  toField (TrxDeclined _)    = Escape "declined"
  toField TrxCompleted       = Escape "completed"
  toField (TrxDisputed _)    = Escape "disputed"

instance ToRow Transaction where
  toRow Transaction {..} =
    [ toField trxId
    , toField trxRevision
    , toField trxVersion
    , toField trxMsgSource
    , case trxState of
      TrxCreated         -> Escape "created"
      TrxAuthorized      -> Escape "authorized"
      TrxPending         -> Escape "pending"
      TrxPendingReversal -> Escape "pendingReversal"
      TrxDeclined _      -> Escape "declined"
      TrxCompleted       -> Escape "completed"
      TrxDisputed _      -> Escape "disputed"
    , toField $ case trxState of
      TrxCreated         -> Nothing
      TrxAuthorized      -> Nothing
      TrxPending         -> Nothing
      TrxPendingReversal -> Nothing
      TrxDeclined _      -> Nothing
      TrxCompleted       -> Nothing
      TrxDisputed d      -> Just d
    , toField $ case trxState of
      TrxCreated         -> Nothing
      TrxAuthorized      -> Nothing
      TrxPending         -> Nothing
      TrxPendingReversal -> Nothing
      TrxDeclined r      -> Just r
      TrxCompleted       -> Nothing
      TrxDisputed _      -> Nothing
    , toField trxSource
    , toField trxSourceId
    , toField trxSourceEvent
    , toField trxUserId
    , toField trxDisplayAmount
    , toField trxBillingAmounts
    , toField trxDetails
    , toField $ case trxGroupId of
      Just (gid, _) -> Just gid
      Nothing       -> Nothing
    , toField $ case trxGroupId of
      Just (_, rev) -> Just rev
      Nothing       -> Nothing
    , toField trxSourceIdempotency
    , toField trxSplitAmounts
    , toField trxMerchant
    , toField trxDescription
    , toField trxPurchasedAt
    , toField trxAdjustments
    , toField trxRewardId
    ]

instance FromRow Transaction where
  fromRow = do
    trxId                          <- field
    trxRevision                    <- field
    trxVersion                     <- field
    trxMsgSource                   <- field
    state :: Text                  <- field
    dispute :: Maybe CardDispute   <- field
    decline :: Maybe DeclineReason <- field
    trxSource                      <- field
    trxSourceId                    <- field
    trxSourceEvent                 <- field
    trxUserId                      <- field
    trxDisplayAmount               <- field
    trxBillingAmounts              <- field
    trxDetails                     <- field
    groupId                        <- field
    groupRev                       <- field
    trxSourceIdempotency           <- field
    trxSplitAmounts                <- field
    trxMerchant                    <- field
    trxDescription                 <- field
    trxPurchasedAt                 <- field
    trxAdjustments                 <- field
    trxRewardId                    <- field

    let trxState = case (state, dispute, decline) of
          ("created"        , _      , _      ) -> TrxCreated
          ("authorized"     , _      , _      ) -> TrxAuthorized
          ("pending"        , _      , _      ) -> TrxPending
          ("pendingReversal", _      , _      ) -> TrxPendingReversal
          ("declined"       , _      , Just d ) -> TrxDeclined d
          ("declined", _, Nothing) -> error "Declined requires json"
          ("completed"      , _      , _      ) -> TrxCompleted
          ("disputed"       , Just d , _      ) -> TrxDisputed d
          ("disputed", Nothing, _) -> error "Disputed requires json"
          (k, _, _) -> error . T.unpack $ "Unknown state \"" <> k <> "\""

    let trxGroupId = case (groupId, groupRev) of
          (Just g, Just r) -> Just (GroupId g, r)
          (Nothing, Nothing) -> Nothing
          _ -> error "groupId and groupRev must both by Just or Nothing"

    return Transaction { .. }

sqlTransactionSelect :: (Query, Query)
sqlTransactionSelect =
  ( " id, revision, version, msg_source, state, dispute_info, decline_info, source, source_id, source_event, user_id, amount_display, amount_billing, details, group_id, group_revision, source_idempotency, split_amounts, merchant, description, purchased_at, adjustments, reward_id "
  , " ? , ?       , ?      , ?         , ?    , ?           , ?           , ?     , ?        , ?           , ?      , ?             , ?             , ?      , ?       , ?             , ?                 , ?            , ?       , ?          , ?           , ?          , ?         "
  )

data AptoAdjustment = AptoAdjustment
  { adjId              :: AptoAdjustmentID
  , adjCreatedAt       :: UTCTime
  , adjAmountLocal     :: Currency
  , adjAmountBilling   :: Currency
  , adjFSTransactionId :: Maybe Text
  , adjType            :: AptoAdjustmentType
  }
  deriving (Eq, Show, Generic)
instance FromJSON AptoAdjustment where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON AptoAdjustment where
  toJSON = genericToJSON customAesonOptions

newtype AptoAdjustmentID = AptoAdjustmentID Text deriving (Eq, Show, Generic, FromJSON, ToJSON)
data AptoAdjustmentType
  = CaptureAdjustment
  | RefundAdjustment
  | AuthorizationAdjustment
  deriving (Eq, Show, Generic)
instance FromJSON AptoAdjustmentType where
  parseJSON = withText "AptoAdjustment" $ \t -> case t of
    "capture"       -> return CaptureAdjustment
    "refund"        -> return RefundAdjustment
    "authorization" -> return AuthorizationAdjustment
    k -> fail $ "Did not recognize " <> T.unpack k <> " as AptoAdjustmentType"
instance ToJSON AptoAdjustmentType where
  toJSON CaptureAdjustment       = "capture"
  toJSON RefundAdjustment        = "refund"
  toJSON AuthorizationAdjustment = "authorization"
