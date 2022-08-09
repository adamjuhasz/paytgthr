{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Referral.ReferralProgram where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
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
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( ReferralProgramID
                                                , RewardId
                                                )

data ReferralWorkflow
  = Immediate -- Represents a "promo code"
  | PurchaseCount
    { qualifyingPurchaseMin :: Currency
    , minCount :: Int
    , timeLimitDays :: Int
    }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReferralPayout
  = CashPayout Currency
  | BoostPayout RewardId
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance FromField ReferralPayout where
  fromField = fromJSONField
instance ToField ReferralPayout where
  toField = toJSONField

newtype RewardLevels = RewardLevels [(Int, ReferralPayout)]
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ReferralProgram = ReferralProgram
  { refProgram     :: ReferralProgramID
  , refWorkflow    :: ReferralWorkflow
  , referrerReward :: RewardLevels
  , refereeReward  :: ReferralPayout
  , refActive      :: Bool -- Set by admin to disable any new refree using this
  , refOpenAccess  :: Bool -- If True then any new refree can join program
  , refCreatedAt   :: UTCTime
  , refUpdatedAt   :: UTCTime
  , refRevision    :: Int
  , refName        :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance FromField ReferralWorkflow where
  fromField = fromJSONField
instance ToField ReferralWorkflow where
  toField = toJSONField

instance FromField RewardLevels where
  fromField = fromJSONField
instance ToField RewardLevels where
  toField = toJSONField

instance ToRow ReferralProgram where
  toRow ReferralProgram {..} =
    [ toField refProgram
    , toField refWorkflow
    , toField referrerReward
    , toField refereeReward
    , toField refActive
    , toField refOpenAccess
    , toField refCreatedAt
    , toField refUpdatedAt
    , toField refRevision
    , toField refName
    ]

instance FromRow ReferralProgram where
  fromRow = do
    refProgram     <- field
    refWorkflow    <- field
    referrerReward <- field
    refereeReward  <- field
    refActive      <- field
    refOpenAccess  <- field
    refCreatedAt   <- field
    refUpdatedAt   <- field
    refRevision    <- field
    refName        <- field

    return ReferralProgram { .. }

referralProgramFields :: (Query, Query)
referralProgramFields =
  ( " id, workflow, reward_referrer, reward_referee, active, openaccess, created_at, updated_at, revision, name "
  , " ? , ?       , ?              , ?             , ?     , ?         , ?         , ?         , ?       , ?    "
  )
