{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use lambda-case" -}

module Shared.Models.Apto.Cardholder where

import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (<?>)
                                                , FromJSON(..)
                                                , KeyValue(..)
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                , withText
                                                )
import           Data.Aeson.Types               ( JSONPathElement(Key)
                                                , Parser
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           Shared.Models.Apto.Base        ( AptoCardholderId )
import           Shared.Models.Apto.Card        ( AptoCard )
import           Shared.Models.Card             ( CardDesign )
import           Shared.Models.KYC              ( KYCFailureReasons(..)
                                                , KycStatus(..)
                                                )
import           Shared.Models.User             ( EmailAddress
                                                , UserID
                                                )

data AptoAddress = AptoAddress
  { addStreetOne  :: Text
  , addStreetTwo  :: Maybe Text
  , addLocality   :: Text -- City
  , addRegion     :: Text -- State
  , addPostalCode :: Text
  , addCountry    :: Text
  }
  deriving (Eq, Show)
instance ToJSON AptoAddress where
  toJSON AptoAddress {..} = object
    [ "street_one" .= addStreetOne
    , "street_two" .= addStreetTwo
    , "locality" .= addLocality
    , "region" .= addRegion
    , "postal_code" .= addPostalCode
    , "country" .= addCountry
    ]

data AptoDocument = AptoDocument
  { adoType  :: Text -- "SSN"
  , adoValue :: Text -- "123456789"
  }
  deriving (Eq, Show)

newtype Year = Year Text deriving (Eq, Show)
newtype Month = Month Text deriving (Eq, Show)
newtype Day = Day Text deriving (Eq, Show)

data AptoCardholderRequest = AptoCardholderRequest
  { accrTgthrId     :: UserID
  , accrNameFirst   :: Text
  , accrNameLast    :: Text
  , accrEmail       :: EmailAddress
  , accrDOB         :: UTCTime
  , accrPhoneNumber :: Text
  , accrAddress     :: AptoAddress
  , accrSSN         :: Text
  , accrCardDesign  :: CardDesign
  }
  deriving (Eq, Show)
instance ToJSON AptoCardholderRequest where
  toJSON AptoCardholderRequest {..} = object
    [ "first_name" .= accrNameFirst
    , "last_name" .= accrNameLast
    , "custodian_uid" .= accrTgthrId
    , "email" .= accrEmail
    , "phone_number" .= accrPhoneNumber
    , "date_of_birth" .= formatTime defaultTimeLocale "%Y-%m-%d" accrDOB
    , "address" .= accrAddress
    , "document" .= object ["type" .= ("SSN" :: Text), "value" .= accrSSN]
    , "card" .= object ["design_key" .= accrCardDesign]
    , ("program_id", "Paytogether")
    ]

data KYCEvent
  = KYCStatusUpdate   -- user pending / passed / failed kyc
  | KYCUserUpdate     -- user data changed
  | KYCIdentityUpdate -- user data changed
  deriving (Eq, Show)
instance FromJSON KYCEvent where
  parseJSON = withText "KYCEvent" $ \t -> case t of
    "kyc_status_update" -> return KYCStatusUpdate
    "user_update"       -> return KYCUserUpdate
    "identity_update"   -> return KYCIdentityUpdate
    _                   -> fail $ "Unknown KYCEvent: \"" <> T.unpack t <> "\""

data AptoCardholderResponse = AptoCardholderResponse
  { accxId          :: AptoCardholderId
  , accxEmail       :: Text
  , accxKYCStatus   :: Maybe KycStatus
  , accxNameFirst   :: Text
  , accxNameLast    :: Text
  , acxcPhoneNumber :: Text
  , accxPayTgthrId  :: Text
  , accxCreatedAt   :: UTCTime
  , accxCards       :: [AptoCard]
  , accxEvent       :: Maybe KYCEvent
  , accxLiveMode    :: Bool
  , accxKYCPassedAt :: Maybe UTCTime
  }
  deriving (Eq, Show)
instance FromJSON AptoCardholderResponse where
  parseJSON = withObject "AptoCardholderResponse" $ \o -> do
    cd <- o .: "cardholder"
    identityReason :: [KYCFailureReasons] <-
      (cd .: "kyc_identity_reason" <?> Key "cardholder" :: Parser (Maybe Text))
        >>= \x -> case x of
              Nothing                   -> return []
              Just "WATCHLIST_MATCH"    -> return [WatchlistRejected]
              Just "PHONE_MISMATCH"     -> return [PhoneScoreLow]
              Just "SSN_INVALID"        -> return [SSNScoreLow]
              Just "SSN_MISMATCH"       -> return [SSNScoreLow]
              Just "INCOMPLETE_PROFILE" -> return [DocumentationIncorrect]
              Just "IDENTITY_UPDATE"    -> return [DocumentationIncorrect]
              Just t ->
                fail $ "KYC identity reason of " <> show t <> " not understood"

    addressReason :: [KYCFailureReasons] <-
      (cd .:? "kyc_address_reason" <?> Key "cardholder" :: Parser (Maybe Text))
        >>= \x -> case x of
              Nothing                   -> return []
              Just "INCOMPLETE_PROFILE" -> return [DocumentationIncorrect]
              Just "ADDRESS_RISKY"      -> return [AddressScoreLow]
              Just "ADDRESS_PO_BOX"     -> return [AddressScoreLow]
              Just "ADDRESS_UPDATE"     -> return [AddressScoreLow]
              Just "ADDRESS_COMMERCIAL" -> return [AddressScoreLow]
              Just t ->
                fail $ "KYC address reason of " <> show t <> " not understood"

    fileReason :: [KYCFailureReasons] <-
      (cd .:? "kyc_file_reason" <?> Key "cardholder" :: Parser (Maybe Text))
        >>= \x -> case x of
              Nothing                  -> return []
              Just "INSUFFICIENT_FILE" -> return [DocumentationIncorrect]
              Just "UNRELATED_FILE"    -> return [DocumentationIncorrect]
              Just "WRONG_FILE"        -> return [DocumentationIncorrect]
              Just "BLURRY"            -> return [DocumentationIncorrect]
              Just "REQUIRES_ADDITIONAL_INFORMATION" ->
                return [DocumentationIncorrect]
              Just t ->
                fail $ "KYC file reason of " <> show t <> " not understood"

    let failReasosn = identityReason <> addressReason <> fileReason

    accxId          <- cd .: "id" <?> Key "cardholder"
    accxEmail       <- cd .: "email" <?> Key "cardholder"
    accxNameFirst   <- cd .: "first_name" <?> Key "cardholder"
    accxNameLast    <- cd .: "last_name" <?> Key "cardholder"
    acxcPhoneNumber <- cd .: "phone" <?> Key "cardholder"
    accxPayTgthrId  <- cd .: "custodian_uid" <?> Key "cardholder"
    accxCreatedAt   <- cd .: "created_at" <?> Key "cardholder"
    accxKYCStatus   <-
      (cd .: "kyc_status" <?> Key "cardholder" :: Parser (Maybe Text))
        >>= \x -> case x of
              Nothing       -> return Nothing
              Just "PASSED" -> return $ Just Passed
              Just "NOT_CHECKED" ->
                return $ Just $ AutoVerifyFailed failReasosn
              Just "UPLOAD_FILE" ->
                return $ Just $ AutoVerifyFailed failReasosn
              Just "UNDER_REVIEW" ->
                return $ Just $ AutoVerifyFailed failReasosn
              Just "RESUBMIT_DETAILS" ->
                return $ Just $ AutoVerifyFailed failReasosn
              Just "NOT_APPLICABLE" ->
                return $ Just $ AutoVerifyFailed failReasosn
              Just "TEMPORARY_ERROR" ->
                return $ Just $ AutoVerifyFailed failReasosn
              Just "REJECTED" -> return $ Just $ Rejected failReasosn
              t -> fail $ "KYC Status of " <> show t <> " not understood"

    accxCards       <- cd .: "cards" <?> Key "cardholder"
    accxEvent       <- cd .: "event" <?> Key "cardholder"
    accxLiveMode    <- cd .: "livemode" <?> Key "cardholder"
    accxKYCPassedAt <- cd .: "kyc_passed_at" <?> Key "cardholder"

    return AptoCardholderResponse { .. }
