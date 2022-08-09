{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Shared.Models.KYCAssesment
  ( module Shared.Models.KYCAssesment
  , KYCFailureReasons(..)
  ) where

import           Data.Aeson                     ( FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..)
                                                , fromJSONField
                                                )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..)
                                                , toJSONField
                                                )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import qualified Database.PostgreSQL.Simple.Types
                                               as PT
import           GHC.Generics                   ( Generic )
import           Shared.Models.KYC              ( KYCFailureReasons(..) )
import           Shared.Models.User             ( RedactedText
                                                , UserID
                                                )

newtype CognitoProfileId = CognitoProfileId Text deriving (Eq, Show)
instance FromJSON CognitoProfileId where
  parseJSON t = CognitoProfileId <$> parseJSON t
instance ToJSON CognitoProfileId where
  toJSON (CognitoProfileId t) = toJSON t
instance ToField CognitoProfileId where
  toField (CognitoProfileId t) = toField t
instance FromField CognitoProfileId where
  fromField a dat = CognitoProfileId <$> fromField a dat

newtype IdentitySearchId = IdentitySearchId Text deriving (Eq, Show)
instance FromJSON IdentitySearchId where
  parseJSON t = IdentitySearchId <$> parseJSON t
instance ToJSON IdentitySearchId where
  toJSON (IdentitySearchId t) = toJSON t
instance ToField IdentitySearchId where
  toField (IdentitySearchId t) = toField t
instance FromField IdentitySearchId where
  fromField a dat = IdentitySearchId <$> fromField a dat

instance ToField [(Text, Int)] where
  toField = toJSONField
instance FromField [(Text, Int)] where
  fromField = fromJSONField

instance ToField [(RedactedText, Int)] where
  toField = toJSONField
instance FromField [(RedactedText, Int)] where
  fromField = fromJSONField

data KYCAssesment = KYCAssesment
  { userId         :: UserID
  , profileId      :: CognitoProfileId
  , searchId       :: IdentitySearchId
  , kycPassed      :: Bool
  , scoreOverall   :: Int
  , failureReasons :: [KYCFailureReasons]
  , createdAt      :: UTCTime
  -- Average scores for each trait
  , scorePhone     :: Maybe Int
  , scoreAddress   :: Maybe Int
  , scoreSSN       :: Maybe Int
  , scoreName      :: Maybe Int
  -- Count of results for each trait
  , countPhone     :: Int
  , countAddresss  :: Int
  , countSSN       :: Int
  , countName      :: Int
  -- Inputs we sent to the search
  , inputPhone     :: Maybe Text
  , inputAddress   :: Maybe Text
  , inputSSN       :: Maybe RedactedText
  , inputName      :: Maybe Text
  -- rawResults
  , sourcePhone    :: [(Text, Int)]
  , sourceAddress  :: [(Text, Int)]
  , sourceSSN      :: [(RedactedText, Int)]
  , sourceName     :: [(Text, Int)]
  }
  deriving (Eq, Show, Generic)
instance FromJSON KYCAssesment where
instance ToJSON KYCAssesment where

instance ToRow KYCAssesment where
  toRow KYCAssesment {..} =
    [ toField userId
    , toField profileId
    , toField searchId
    , toField kycPassed
    , toField scoreOverall
    , toField failureReasons
    , toField createdAt
    -- scores
    , toField scorePhone
    , toField scoreAddress
    , toField scoreSSN
    , toField scoreName
    -- counts
    , toField countPhone
    , toField countAddresss
    , toField countSSN
    , toField countName
    -- inputs
    , toField inputPhone
    , toField inputAddress
    , toField inputSSN
    , toField inputName
    -- sources
    , toField sourcePhone
    , toField sourceAddress
    , toField ([] :: [(RedactedText, Int)])
    , toField sourceName
    ]

instance FromRow KYCAssesment where
  fromRow = do
    userId         <- field
    profileId      <- field
    searchId       <- field
    kycPassed      <- field
    scoreOverall   <- field
    failureReasons <- field
    createdAt      <- field
    -- scores
    scorePhone     <- field
    scoreAddress   <- field
    scoreSSN       <- field
    scoreName      <- field
    -- counts
    countPhone     <- field
    countAddresss  <- field
    countSSN       <- field
    countName      <- field
    --inputs used
    inputPhone     <- field
    inputAddress   <- field
    inputSSN       <- field
    inputName      <- field
    -- source responses
    sourcePhone    <- field
    sourceAddress  <- field
    sourceSSN      <- field
    sourceName     <- field

    return KYCAssesment { .. }

kycAssesmentFields :: (PT.Query, PT.Query)
kycAssesmentFields =
  ( " user_id, profile_id, search_id, passed, score_overall, failure_reasons, created_at, score_phone, score_address, score_ssn, score_name, count_phone, count_address, count_ssn, count_name, input_phone, input_address, input_ssn, input_name, source_phone, source_address, source_ssn, source_name "
  , " ?      , ?         , ?        , ?     , ?            , ?              , ?         , ?          , ?            , ?        , ?         , ?          , ?            , ?        , ?         , ?          , ?            , ?        , ?         , ?           , ?             , ?         , ?           "
  )
