{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Rewards.Activation where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Database.PostgreSQL.Simple.FromField
                                                ( FromField(..) )
import           Database.PostgreSQL.Simple.FromRow
                                                ( FromRow(..)
                                                , field
                                                )
import           Database.PostgreSQL.Simple.ToField
                                                ( ToField(..) )
import           Database.PostgreSQL.Simple.ToRow
                                                ( ToRow(..) )
import           Database.PostgreSQL.Simple.Types
                                                ( Query )
import           GHC.Generics                   ( Generic )
import           Shared.Models.Ids              ( ActivatedRewardId
                                                , GroupId
                                                , RewardId
                                                , UserID
                                                )

data ActivationState
  = BoostActive
  | BoostCancelled
  | BoostExpired
  | BoostUsed
   deriving (Eq, Show, Read, Generic, FromJSON, ToJSON)

data RewardBoostActivation = RewardBoostActivation
  { activationId        :: ActivatedRewardId
  , activatedReward     :: RewardId
  , activatedGroup      :: GroupId
  , activationCreatedAt :: UTCTime
  , activationUpdatedAt :: UTCTime
  , activationChangeBy  :: UserID
  , activationExpires   :: UTCTime
  , activationState     :: ActivationState
  , activationUsesLeft  :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToField ActivationState where
  toField s = toField $ show s
instance FromField ActivationState where
  fromField a dat = read <$> fromField a dat

instance ToRow RewardBoostActivation where
  toRow RewardBoostActivation {..} =
    [ toField activationId
    , toField activatedReward
    , toField activatedGroup
    , toField activationCreatedAt
    , toField activationUpdatedAt
    , toField activationChangeBy
    , toField activationExpires
    , toField activationState
    , toField activationUsesLeft
    ]

instance FromRow RewardBoostActivation where
  fromRow = do
    activationId        <- field
    activatedReward     <- field
    activatedGroup      <- field
    activationCreatedAt <- field
    activationUpdatedAt <- field
    activationChangeBy  <- field
    activationExpires   <- field
    activationState     <- field
    activationUsesLeft  <- field

    return RewardBoostActivation { .. }

activationFields :: (Query, Query)
activationFields =
  ( " id, reward_id, group_id, created_at, updated_at, changed_by_id, expires_at, state, uses_left "
  , " ? , ?        , ?       , ?         , ?         , ?            , ?         , ?    , ?         "
  )
