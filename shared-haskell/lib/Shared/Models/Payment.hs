{- HLINT ignore "Use let" -}
{- HLINT ignore "Use lambda-case" -}

{-# LANGUAGE StrictData, DeriveGeneric, DeriveAnyClass, RecordWildCards, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Models.Payment
  ( module Shared.Models.Payment
  , PaymentId(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , genericParseJSON
                                                , genericToJSON
                                                )
import qualified Data.ByteString.Char8         as BC8
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
import           Servant                        ( FromHttpApiData(parseUrlPiece)
                                                , ToHttpApiData(toUrlPiece)
                                                )
import           Shared.Models.Base             ( Revision )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids              ( JournalId
                                                , MessageID(..)
                                                , PaymentId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.User             ( RedactedText )
import           Shared.Utils                   ( customAesonOptions
                                                , stringToTime
                                                )
import           Text.Read                      ( readEither
                                                , readMaybe
                                                )

examplePayment :: IO Payment
examplePayment = do
  pid <- nextRandom <&> PaymentId
  mid <- nextRandom <&> MessageID
  uid <- nextRandom <&> UserID

  return Payment
    { payId          = pid
    , payRevision    = 1
    , payVersion     = "1.0"
    , payMsgSource   = mid
    , payStatus      = PaymentCreated
    , payUser        = uid
    , payType        = DebitFromUser
    , payMethod      = DwollaSettlement
    , payMethodId    =
      Just
        "https://api-sandbox.dwolla.com/transfers/00000000-0000-0000-0000-000000000000"
    , payAmount      = Currency "USD" 100.01
    , payText        = "Tgthr - Target Inc"
    , paySubType     = NormalPayment
    , payACHInfo     = Just ("123456789", "1345678")
    , payCreatedAt   = stringToTime "2019-09-02T20:15:32+00:00"
    , payFromJournal = Nothing
    , payToJournal   = Nothing
    }

data PaymentSubType
  = InitialVerification -- do not alert to user
  | RefundVerification -- do not alert to user
  | NormalPayment
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField PaymentSubType where
  toField = toJSONField
instance FromField PaymentSubType where
  fromField = fromJSONField

type ABARoutingNum = RedactedText
type DDAAccountNum = RedactedText

data Payment = Payment
  { payId          :: PaymentId
  , payRevision    :: Revision
  , payVersion     :: Text
  , payMsgSource   :: MessageID
  , payStatus      :: PaymentStatus
  , payUser        :: UserID
  , payType        :: PaymentType
  , payMethod      :: PaymentMethod
  , payMethodId    :: Maybe Text
  , payAmount      :: Currency
  , payText        :: Text
  , paySubType     :: PaymentSubType
  , payACHInfo     :: Maybe (ABARoutingNum, DDAAccountNum)
  , payCreatedAt   :: UTCTime
  , payFromJournal :: Maybe JournalId
  , payToJournal   :: Maybe JournalId
  }
  deriving (Eq, Show, Generic)
instance FromJSON Payment where
  parseJSON = genericParseJSON customAesonOptions
instance ToJSON Payment where
  toJSON = genericToJSON customAesonOptions

instance ToField (ABARoutingNum, DDAAccountNum) where
  toField = toJSONField
instance FromField  (ABARoutingNum, DDAAccountNum) where
  fromField = fromJSONField

data PaymentType
  = DebitFromUser
  | CreditToUser
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance ToField PaymentType where
  toField DebitFromUser = Escape "debit"
  toField CreditToUser  = Escape "credit"
instance FromField PaymentType where
  fromField a dat = fromField a dat <&> \t -> case t of
    "debit"  -> DebitFromUser
    "credit" -> CreditToUser
    v        -> error $ "Unknown PaymentType of \"" <> v <> "\""

data PaymentFailureCode
  = ACHR01 -- Insufficient Funds
  | ACHR02 -- Account Closed
  | ACHR03 -- No Account/Unable to Locate Account
  | ACHR04 -- Invalid Account Number
  | ACHR05 -- Unauthorized Debit Entry
  | ACHR06 -- Returned per ODFI’s Request
  | ACHR07 -- Authorization Revoked by Customer (adjustment entries)
  | ACHR08 -- Payment Stopped or Stop Payment on Item
  | ACHR09 -- Uncollected Funds
  | ACHR10 -- Customer Advises Not Authorized; Item Is Ineligible, Notice Not Provided, Signatures Not Genuine, or Item Altered (adjustment entries)
  | ACHR11 -- Customer Advises Entry Not in Accordance with the Terms of the Authorization
  | ACHR12 -- Branch Sold to Another DFI
  | ACHR13 -- RDFI not qualified to participate
  | ACHR14 -- Representative Payee Deceased or Unable to Continue in that Capacity
  | ACHR15 -- Beneficiary or Account Holder (Other Than a Representative Payee) Deceased
  | ACHR16 -- Account Frozen
  | ACHR17 -- File Record Edit Criteria (Specify)
  | ACHR20 -- Non-Transaction Account
  | ACHR21 -- Invalid Company Identification
  | ACHR22 -- Invalid Individual ID Number
  | ACHR23 -- Credit Entry Refused by Receiver
  | ACHR24 -- Duplicate Entry
  | ACHR29 -- Corporate Customer Advises Not Authorized
  | ACHR31 -- Permissible Return Entry (CCD and CTX only)
  | ACHR33 -- Return of XCK Entry
  | ACHUnknown Text
  deriving (Eq, Show, Read, Ord, Generic, FromJSON, ToJSON)
instance ToField PaymentFailureCode where
  toField = toJSONField
instance FromField PaymentFailureCode where
  fromField = fromJSONField

-- | Apply a function to a 'Left' constructor
mapLeft :: (a1 -> a2) -> Either a1 b -> Either a2 b
mapLeft f (Left  a1) = Left (f a1)
mapLeft _ (Right b ) = Right b

data PaymentStatus
  = PaymentCreated
  | PaymentPending
  | PaymentCompleted
  | PaymentFailed PaymentFailureCode
  | PaymentCancelled
  deriving (Eq, Show, Read, Ord, Generic, FromJSON, ToJSON)
instance ToHttpApiData PaymentStatus where
  toUrlPiece = T.pack . show
instance FromHttpApiData PaymentStatus where
  parseUrlPiece = mapLeft T.pack . readEither . T.unpack

data PaymentMethod
    = DwollaSettlement
    | DwollaACH Text
  deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)
instance ToField PaymentMethod where
  toField DwollaSettlement = Escape "dwolla-ach"
  toField x                = Escape $ BC8.pack $ show x

instance FromField PaymentMethod where
  fromField a dat = fromField a dat <&> \t -> case t of
    "dwolla-ach" -> DwollaSettlement
    v -> fromMaybe (error $ "Unknown PaymentMethod of \"" <> v <> "\"")
                   (readMaybe v)

instance ToRow Payment where
  toRow Payment {..} =
    [ toField payId
    , toField payRevision
    , toField payVersion
    , toField payMsgSource
    , toField $ case payStatus of
      PaymentCreated   -> Escape "created"
      PaymentPending   -> Escape "pending"
      PaymentCompleted -> Escape "completed"
      PaymentFailed _  -> Escape "failed"
      PaymentCancelled -> Escape "cancelled"
    , toField payUser
    , toField payType
    , toField payMethod
    , toField payAmount
    , toField payText
    , toField payMethodId
    , toField $ case paySubType of -- payVisible (deprecated by paySubType)
      NormalPayment       -> True
      InitialVerification -> False
      RefundVerification  -> False
    , toField paySubType
    , toField payACHInfo
    , toField $ case payStatus of
      PaymentFailed r -> Just r
      _               -> Nothing
    , toField payCreatedAt
    , toField payFromJournal
    , toField payToJournal
    ]

instance FromRow Payment where
  fromRow = do
    payId               <- field
    payRevision         <- field
    payVersion          <- field
    payMsgSource        <- field
    status :: String    <- field
    payUser             <- field
    payType             <- field
    payMethod           <- field
    payAmount           <- field
    payText             <- field
    payMethodId         <- field
    _payVisible :: Bool <- field
    paySubType          <- field
    payACHInfo          <- field
    payFailure          <- field
    payCreatedAt        <- field
    payFromJournal      <- field
    payToJournal        <- field

    payStatus           <- return $ case (status, payFailure) of
      ("created"  , _      ) -> PaymentCreated
      ("pending"  , _      ) -> PaymentPending
      ("completed", _      ) -> PaymentCompleted
      ("failed"   , Just r ) -> PaymentFailed r
      ("failed"   , Nothing) -> PaymentFailed (ACHUnknown "Unknown")
      ("cancelled", _      ) -> PaymentCancelled
      (k, _) -> error $ "Payment payStatus unknown \"" <> k <> "\""

    return Payment { .. }

sqlPaymentSelect :: (Query, Query)
sqlPaymentSelect =
  ( " id, revision, version, msg_source, status, user_id, pay_type, pay_method, pay_amount, description, pay_method_id, \"visible\", pay_subtype, pay_achinfo, pay_failurecode, created_at, journal_from_id, journal_to_id "
  , " ? , ?       , ?      , ?         , ?     , ?       , ?      , ?         , ?         , ?          , ?            , ?          , ?          , ?          , ?              , ?         , ?              , ?             "
  )
