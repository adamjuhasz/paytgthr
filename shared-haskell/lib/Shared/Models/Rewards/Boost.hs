{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Shared.Models.Rewards.Boost where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.List.NonEmpty             ( NonEmpty )
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
import           Shared.Models.Ids              ( RewardId )
import           Shared.Models.Transaction      ( MastercardMCC )

data BoostMatcher
  = MatchMCC MastercardMCC
  | MatchText Text
  | MatchAND (NonEmpty BoostMatcher)
  | MatchOR (NonEmpty BoostMatcher)
  | MatchNOT BoostMatcher
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data RewardBoost = RewardBoost
  { boostId            :: RewardId
  , boostMatch         :: BoostMatcher
  , boostRewardInBips  :: Integer
  , boostExpiresInHr   :: Integer
  , boostName          :: Text
  , boostMaximumPayout :: Currency
  , boostActive        :: Bool
  , boostCreated       :: UTCTime
  , boostUpdated       :: UTCTime
  , boostUses          :: Maybe Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
instance Ord RewardBoost where
  compare a b
    | boostRewardInBips a == boostRewardInBips b = compare (boostId a)
                                                           (boostId b)
    | otherwise = compare (boostRewardInBips a) (boostRewardInBips b)

instance FromField BoostMatcher where
  fromField = fromJSONField
instance ToField BoostMatcher where
  toField = toJSONField

instance ToRow RewardBoost where
  toRow RewardBoost {..} =
    [ toField boostId
    , toField boostMatch
    , toField boostRewardInBips
    , toField boostExpiresInHr
    , toField boostName
    , toField boostMaximumPayout
    , toField boostActive
    , toField boostCreated
    , toField boostUpdated
    , toField boostUses
    ]

instance FromRow RewardBoost where
  fromRow = do
    boostId            <- field
    boostMatch         <- field
    boostRewardInBips  <- field
    boostExpiresInHr   <- field
    boostName          <- field
    boostMaximumPayout <- field
    boostActive        <- field
    boostCreated       <- field
    boostUpdated       <- field
    boostUses          <- field

    return RewardBoost { .. }

rewardFields :: (Query, Query)
rewardFields =
  ( " id, matcher, reward, expires_in_hr, name, max_payout, active, created_at, updated_at, uses "
  , " ? , ?      , ?     , ?            , ?   , ?         , ?     , ?         , ?         , ?    "
  )
