{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveAnyClass, DeriveGeneric, StrictData #-}


module APIApto.Model where

import           Data.Aeson                    as A
                                                ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , Object
                                                , Options(..)
                                                , ToJSON(toJSON)
                                                , Value(Object, String)
                                                , camelTo2
                                                , defaultOptions
                                                , genericToJSON
                                                , object
                                                , withObject
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Apto.Transaction ( AptoTransactionId(..)
                                                , MerchantInfo(..)
                                                , TransactionDetails(..)
                                                )
import           Shared.Models.Card             ( AptoCardId(..) )
import qualified Shared.Models.Transaction     as Trx
import           Shared.Models.Transaction      ( CardNetwork(..)
                                                , DeclineReason(..)
                                                , MastercardMCC(MastercardMCC)
                                                , TransactionId
                                                )
import           Text.Read                      ( readMaybe )

customAesonOptions :: A.Options
customAesonOptions = A.defaultOptions
  { constructorTagModifier = camelTo2 '_'
  , fieldLabelModifier     = camelTo2 '_' . drop 3
  }

newtype IdempotencyKey = IdempotencyKey Text deriving (Eq, Show, Generic, FromJSON, ToJSON)

data TrxDirection
  = DebitTransaction
  | CreditTransaction
  deriving (Eq, Show)

parseTranactionAuthRequestV1 :: Object -> Parser TranactionAuthRequest
parseTranactionAuthRequestV1 o = do
  trxType :: Text <- o .: "type"
  tarType         <- case trxType of
    "debit"  -> return DebitTransaction
    "credit" -> return CreditTransaction
    _        -> fail "Unknown trx type"
  tarAmount         <- o .: "amount"
  tarCurrency       <- o .: "currency"
  tarTransactionId  <- AptoTransactionId <$> o .: "transaction_id"
  tarIdempotencyKey <- IdempotencyKey <$> o .: "idempotency_key"
  tarDescription    <- o .: "merchant"

  let tarMerchant     = Nothing
  let tarDetails      = Nothing
  let tarCardholderId = Nothing
  let tarCardId       = Nothing

  return TranactionAuthRequest { .. }

parseTranactionAuthRequestV2 :: Object -> Parser TranactionAuthRequest
parseTranactionAuthRequestV2 o = do
  trxType :: Text <- o .: "type"
  tarType         <- case trxType of
    "debit"  -> return DebitTransaction
    "credit" -> return CreditTransaction
    _        -> fail "Unknown trx type"
  tarAmount         <- o .: "amount"
  tarCurrency       <- o .: "currency"
  tarTransactionId  <- AptoTransactionId <$> o .: "transaction_id"
  tarIdempotencyKey <- IdempotencyKey <$> o .: "idempotency_key"
  tarDescription    <- o .: "merchant"

  cmiName           <- o .: "merchant_name"
  cmiLocality       <- o .: "merchant_locality"
  cmiRegion         <- o .: "merchant_region"
  cmiCountry        <- o .: "merchant_country"
  cmiMcc            <- o .: "mcc"
  cmiMccDesc        <- fromMaybe "Missing" <$> o .: "mcc_name" -- Remove after Apto fixes their systems
  let tarMerchant = Just $ CardMerchant { .. }

  pcpIsCardPresent   <- fromMaybe False <$> o .: "card_a" -- Remove after Apto fixes their systems
  pcpIsOnline        <- fromMaybe True <$> o .: "ecommerce" -- Remove after Apto fixes their systems
  pcpIsInternational <- fromMaybe False <$> o .: "international" -- Remove after Apto fixes their systems
  pcpIsEMV           <- fromMaybe False <$> o .: "emv" -- Remove after Apto fixes their systems
  pcpNetwork         <- o .: "network"
  let pcpType        = Nothing
      pcpContext     = "Authorization Request (PayTgthr internal)"
      pcpDescription = Nothing
      tarDetails     = Just $ CardTransaction { .. }

  let tarCardholderId = Nothing
  let tarCardId       = Nothing

  return TranactionAuthRequest { .. }

parseTranactionAuthRequestV3 :: Object -> Parser TranactionAuthRequest
parseTranactionAuthRequestV3 authObj = do
  obj                    <- authObj .: "authorization"

  billing_amount         <- obj .: "billing_amount"
  tarAmountDbl :: Double <- asum
    [ billing_amount .: "amount" -- The simple “number” case.
    , do
      s <- billing_amount .: "amount" -- The more complicated “string” case.
      case readMaybe s of
        Nothing -> fail "not a number"
        Just x  -> return x
    ]
  tarCurrency <- fromMaybe "USD" <$> billing_amount .: "currency"
  let tarAmount = T.pack . show $ if tarAmountDbl < 0
        then tarAmountDbl * (-1)
        else tarAmountDbl

  let tarType =
        if tarAmountDbl < 0 then DebitTransaction else CreditTransaction

  tarTransactionId  <- AptoTransactionId <$> obj .: "transaction_id"
  tarIdempotencyKey <- IdempotencyKey <$> obj .: "id"

  merchantData      <- obj .: "merchant_data"
  tarDescription    <- fromMaybe "Merchant" <$> merchantData .: "merchant_name"

  tarMerchant       <- asum
    [ Just <$> withObject
      "CardMerchant"
      (\merchant_data -> do
        cmiName     <- merchant_data .: "merchant_name"
        cmiLocality <- merchant_data .: "merchant_locality"
        cmiRegion   <- merchant_data .: "merchant_region"
        cmiCountry  <- merchant_data .: "merchant_country"
        mcc :: Int  <- asum
          [ merchant_data .: "mcc"
          , do
            s <- merchant_data .: "mcc" -- The more complicated “string” case.
            case readMaybe s of
              Nothing -> fail "not a number"
              Just x  -> return x
          ]
        let cmiMcc = MastercardMCC . T.pack $ show mcc
        cmiMccDesc <- fromMaybe "Unknown" <$> merchant_data .: "mcc_group"

        return CardMerchant { .. }
      )
      (Object merchantData)
    , return Nothing
    ]

  transaction_data   <- obj .: "transaction_data"
  pcpIsCardPresent   <- fromMaybe False <$> transaction_data .: "card_a" -- Remove after Apto fixes their systems
  pcpIsOnline        <- fromMaybe True <$> transaction_data .: "ecommerce" -- Remove after Apto fixes their systems
  pcpIsInternational <- fromMaybe False <$> transaction_data .: "international" -- Remove after Apto fixes their systems
  pcpIsEMV           <- fromMaybe False <$> transaction_data .: "emv" -- Remove after Apto fixes their systems
  pcpNetwork         <-
    fromMaybe (UnknownNetwork "null") <$> transaction_data .: "network"
  let pcpType        = Nothing
      pcpContext     = "Authorization Request (PayTgthr internal)"
      pcpDescription = Nothing
  let tarDetails = Just $ CardTransaction { .. }

  tarCardholderId <- Just . AptoCardholderId <$> obj .: "cardholder_id"
  tarCardId       <- Just . AptoCardId <$> obj .: "card_a"

  return TranactionAuthRequest { .. }

data TranactionAuthRequest = TranactionAuthRequest
  { tarType           :: TrxDirection
  , tarAmount         :: Text
  , tarCurrency       :: Text
  , tarTransactionId  :: AptoTransactionId
  , tarIdempotencyKey :: IdempotencyKey
  , tarDescription    :: Text
  , tarMerchant       :: Maybe MerchantInfo
  , tarDetails        :: Maybe TransactionDetails
  , tarCardholderId   :: Maybe AptoCardholderId
  , tarCardId         :: Maybe AptoCardId
  }
  deriving (Eq, Show, Generic)
instance FromJSON TranactionAuthRequest where
  parseJSON = withObject "TranactionAuthRequest" $ \o -> asum
    [ parseTranactionAuthRequestV3 o
    , parseTranactionAuthRequestV2 o
    , parseTranactionAuthRequestV1 o
    ]

data TransactionAuthResponse
  = TransactionAuthResponse
    { taxIdempotencyKey :: IdempotencyKey
    , taxTransactionId :: AptoTransactionId
    , taxTgthrId :: TransactionId
    }
  | TransactionAuthV3Approved
    { taxApproveExtAuthId :: TransactionId
    }
  | TransactionAuthV3Declined
    { taxDeclineCode :: DeclineReason
    , taxDeclineExtAuthId :: TransactionId
    }
  deriving (Eq, Show, Generic)
instance ToJSON TransactionAuthResponse where
  toJSON TransactionAuthResponse {..} = object
    [ "transaction_id" .= taxTransactionId
    , "idempotency_key" .= taxIdempotencyKey
    , "id" .= taxTgthrId
    ]
  toJSON TransactionAuthV3Approved {..} = object
    [ "authorization" .= object
        ["approved" .= True, "external_authorization_id" .= taxApproveExtAuthId]
    ]
  toJSON TransactionAuthV3Declined {..} = object
    [ "authorization" .= object
        [ "approved" .= False
        , ( "decline_reason"
          , case taxDeclineCode of
            LowBalance         _    -> "LowBalance"
            BalanceCheckFail   _    -> "BalanceCheckFail"
            ExceedMaxTrxAmount _    -> "ExceedMaxTrxAmount"
            InvalidMerchant         -> "InvalidMerchant"
            LostorStolenCard        -> "LostorStolenCard"
            InvalidAmount           -> "InvalidAmount"
            InvalidCardNumber       -> "InvalidAmount"
            CardExpired             -> "CardExpired"
            SuspectFraud            -> "SuspectFraud"
            InternationalNotAllowed -> "InternationalNotAllowed"
            PinRetriesExceeded      -> "PinRetriesExceeded"
            IncorrectCVV            -> "IncorrectCVV"
            CardNotActivated        -> "CardNotActivated"
            CardInactive            -> "CardInactive"
            Trx.CardClosed          -> "CardClosed"
            IncorrectAddress        -> "IncorrectAddress"
            IncorrectPin            -> "IncorrectPin"
            GroupError              -> "GroupError"
            UserNotFound    _       -> "UserNotFound"
            PaymentUnlinked _       -> "PaymentUnlinked"
            P2PNotAllowed           -> "P2PNotAllowed"
            RiskyMerchant           -> "RiskyMerchant"
            UserNotActive _         -> "UserNotActive"
            Unknown       _         -> "Unknown"
          )
        , ( "decline_reason_code"
          , case taxDeclineCode of
            LowBalance         _    -> "decline_nsf"
            BalanceCheckFail   _    -> "decline_nsf"
            ExceedMaxTrxAmount _    -> "decline_nsf"
            InvalidMerchant         -> "decline_nsf"
            LostorStolenCard        -> "decline_nsf"
            InvalidAmount           -> "decline_nsf"
            InvalidCardNumber       -> "decline_nsf"
            CardExpired             -> "decline_nsf"
            SuspectFraud            -> "decline_nsf"
            InternationalNotAllowed -> "decline_nsf"
            PinRetriesExceeded      -> "decline_nsf"
            IncorrectCVV            -> "decline_nsf"
            CardNotActivated        -> "decline_nsf"
            CardInactive            -> "decline_nsf"
            Trx.CardClosed          -> "decline_nsf"
            IncorrectAddress        -> "decline_nsf"
            IncorrectPin            -> "decline_nsf"
            GroupError              -> "decline_nsf"
            UserNotFound    _       -> "decline_nsf"
            PaymentUnlinked _       -> "decline_nsf"
            P2PNotAllowed           -> "decline_nsf"
            RiskyMerchant           -> "decline_nsf"
            UserNotActive _         -> "decline_nsf"
            Unknown       _         -> "decline_nsf"
          )
        , "external_authorization_id" .= taxDeclineExtAuthId
        ]
    ]

data ErrorCodes
  = ErrorBadRequest Text
  | ErrorLowBalance
  | ErrorUnAuthorized
  | ErrorUnknownUser
  | ErrorBalanceCheckFailed
  | ErrorDeclined Text
  deriving (Eq, Show)
instance ToJSON ErrorCodes where
  toJSON (ErrorBadRequest msg) =
    object [("code", String "malformed_request"), ("message", String msg)]
  toJSON ErrorLowBalance = object
    [("code", String "low_balance"), ("message", String "Not enough $$$")]
  toJSON ErrorUnAuthorized = object
    [ ("code"   , String "authorization_failed")
    , ("message", String "Check auth values")
    ]
  toJSON ErrorUnknownUser = object
    [("code", String "unknown_user"), ("message", String "User not recognized")]
  toJSON ErrorBalanceCheckFailed = object
    [ ("code"   , String "balance_check_failed")
    , ("message", String "Balance check failed")
    ]
  toJSON (ErrorDeclined reason) =
    object [("code", String "balance_check_failed"), ("message", String reason)]

newtype ErrorEnvelope = ErrorEnvelope
  { envErrors :: [ErrorCodes]
  } deriving (Eq, Show, Generic)
instance ToJSON ErrorEnvelope where
  toJSON = genericToJSON customAesonOptions

newtype User = User { userName :: Text }
  deriving (Eq, Show)
