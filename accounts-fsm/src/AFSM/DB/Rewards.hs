{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NamedFieldPuns #-}

module AFSM.DB.Rewards where

import           Control.Monad                  ( forM )
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Database.PostgreSQL.Simple     ( Connection
                                                , In(In)
                                                , Only(..)
                                                , execute
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids              ( ActivatedRewardId
                                                , GroupId
                                                , RewardId
                                                )
import           Shared.Models.Rewards.Activation
                                                ( ActivationState(..)
                                                , RewardBoostActivation(..)
                                                , activationFields
                                                )
import           Shared.Models.Rewards.Boost    ( RewardBoost
                                                , rewardFields
                                                )

groupGetRewards :: GroupId -> Connection -> IO [RewardBoost]
groupGetRewards gid conn = do
  activations <- getRewardBoostActivations gid [BoostActive] conn
  forM activations $ \RewardBoostActivation { activatedReward } ->
    fromJust <$> getRewardBoost activatedReward conn

saveRewardBoostActivation :: RewardBoostActivation -> Connection -> IO ()
saveRewardBoostActivation activation conn = do
  _ <- execute conn qs activation
  return ()
 where
  qs =
    [sql| UPSERT INTO tgthr.reward_activations (|]
      <> fst activationFields
      <> [sql| ) VALUES ( |]
      <> snd activationFields
      <> ")"

getRewardBoostActivations
  :: GroupId -> [ActivationState] -> Connection -> IO [RewardBoostActivation]
getRewardBoostActivations gid states conn = query conn
                                                  qsActivation
                                                  (gid, In states)
 where
  qsActivation =
    [sql| SELECT |]
      <> fst activationFields
      <> [sql| FROM tgthr.reward_activations WHERE group_id = ? AND state IN ? ORDER BY updated_at ASC |]

getActiveRewards :: Connection -> IO [RewardBoost]
getActiveRewards conn = query_ conn qs
 where
  qs =
    [sql| SELECT |]
      <> fst rewardFields
      <> [sql| FROM tgthr.rewards WHERE active = TRUE |]

saveRewardBoost :: RewardBoost -> Connection -> IO ()
saveRewardBoost boost conn = do
  _ <- execute conn qs boost
  return ()
 where
  qs =
    [sql|UPSERT INTO tgthr.rewards (|]
      <> fst rewardFields
      <> [sql| ) VALUES ( |]
      <> snd rewardFields
      <> ")"

getRewardBoost :: RewardId -> Connection -> IO (Maybe RewardBoost)
getRewardBoost rid conn = listToMaybe <$> query conn qs (Only rid)
 where
  qs =
    [sql| SELECT |]
      <> fst rewardFields
      <> [sql| FROM tgthr.rewards WHERE id = ? |]

getAllRewards :: Connection -> IO [RewardBoost]
getAllRewards conn = query_ conn qs
 where
  qs =
    [sql| SELECT |]
      <> fst rewardFields
      <> [sql| FROM tgthr.rewards ORDER BY created_at DESC |]

getBoostActivation
  :: ActivatedRewardId -> Connection -> IO (Maybe RewardBoostActivation)
getBoostActivation rid conn = listToMaybe <$> query conn qs (Only rid)
 where
  qs =
    [sql| SELECT |]
      <> fst activationFields
      <> [sql| FROM tgthr.reward_activations WHERE id = ? |]
