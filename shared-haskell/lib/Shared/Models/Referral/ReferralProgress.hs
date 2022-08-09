{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Referral.ReferralProgress where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
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
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Ids              ( ReferralProgramID
                                                , ReferralProgressID
                                                , UserID
                                                )

data WorkFlowProgress
  = PurchaseCountProgress
    { refereeMade :: Int
    , programRequires :: Int
    }
  | ProgramCompleted
  | ProgramExpired
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromField WorkFlowProgress where
  fromField = fromJSONField
instance ToField WorkFlowProgress where
  toField = toJSONField

data ReferralProgress = ReferralProgress
  { progressId        :: ReferralProgressID
  , referalProgram    :: ReferralProgramID
  , referee           :: UserID
  , referrer          :: Maybe UserID
  , programExpiration :: Maybe UTCTime
  , progress          :: WorkFlowProgress
  , progressDisplay   :: Int -- 0 -> 100 shown to use
  , progressCreatedAt :: UTCTime
  , progressUpdatedAt :: UTCTime
  , progressRevision  :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToRow ReferralProgress where
  toRow ReferralProgress {..} =
    [ toField progressId
    , toField referalProgram
    , toField referee
    , toField referrer
    , toField programExpiration
    , toField progress
    , toField progressDisplay
    , toField progressCreatedAt
    , toField progressUpdatedAt
    , toField progressRevision
    ]

instance FromRow ReferralProgress where
  fromRow = do
    progressId        <- field
    referalProgram    <- field
    referee           <- field
    referrer          <- field
    programExpiration <- field
    progress          <- field
    progressDisplay   <- field
    progressCreatedAt <- field
    progressUpdatedAt <- field
    progressRevision  <- field

    return ReferralProgress { .. }

referralProgressFields :: (Query, Query)
referralProgressFields =
  ( " id, program_id, referee_id, referrer_id, expiration_at, progress, progress_display, created_at, updated_at, revision "
  , " ? , ?         , ?         , ?          , ?            , ?       , ?               , ?         , ?         , ?        "
  )
