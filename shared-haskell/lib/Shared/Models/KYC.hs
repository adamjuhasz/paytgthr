{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Shared.Models.KYC where

import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , KeyValue((.=))
                                                , ToJSON(toJSON)
                                                , object
                                                , withObject
                                                )
import           Data.Text                      ( Text )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           GHC.Generics                   ( Generic )

getFailureReasons :: KycStatus -> [KYCFailureReasons]
getFailureReasons Passed                = []
getFailureReasons (AutoVerifyFailed rs) = rs
getFailureReasons (Rejected         rs) = rs

data KycStatus
  -- | Happy path
  = Passed
  -- | Auto check Failed, requires customer to upload docs to allow manual verification
  | AutoVerifyFailed [KYCFailureReasons]
  -- | Rejected for specific reason
  | Rejected [KYCFailureReasons]
  deriving (Eq, Show, Generic)
instance FromJSON KycStatus where
  parseJSON = withObject "KycStatus" $ \o -> do
    kind :: Text <- o .: "kind"
    case kind of
      "Passed"           -> return Passed
      "AutoVerifyFailed" -> do
        body <- o .: "body"
        return $ AutoVerifyFailed body
      "Rejected" -> do
        body <- o .: "body"
        return $ Rejected body
      -- Old Strings From Apto KYC Days
      "passed"           -> return Passed
      "rejected"         -> return $ Rejected []
      "filesrequired"    -> return $ AutoVerifyFailed []
      "resubmitrequired" -> return $ AutoVerifyFailed []
      "notchecked"       -> return $ AutoVerifyFailed []
      "temporaryerror"   -> return $ AutoVerifyFailed []
      "underreview"      -> return $ AutoVerifyFailed []
      -- failure
      r                  -> error $ "Error: unknown KycStatus of " <> show r
instance ToJSON KycStatus where
  toJSON Passed = object [("kind", "Passed")]
  toJSON (AutoVerifyFailed reasons) =
    object [("kind", "AutoVerifyFailed"), "body" .= reasons]
  toJSON (Rejected reasons) = object [("kind", "Rejected"), "body" .= reasons]

instance ToField KycStatus where
  toField = toJSONField
instance FromField KycStatus where
  fromField = fromJSONField

data KYCFailureReasons
  = PhoneScoreLow
  | AddressScoreLow
  | SSNScoreLow
  | NameScoreLow
  | DOBScoreLow
  | WatchlistNeedsReview
  | WatchlistRejected
  | DocumentationIncorrect
  | IdentityTheftRisk
  deriving(Eq, Show, Generic)

instance FromJSON KYCFailureReasons where
instance ToJSON KYCFailureReasons where

instance ToField [KYCFailureReasons] where
  toField = toJSONField
instance FromField [KYCFailureReasons] where
  fromField = fromJSONField
