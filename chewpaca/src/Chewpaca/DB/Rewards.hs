{-# LANGUAGE QuasiQuotes #-}

module Chewpaca.DB.Rewards where

import           Database.PostgreSQL.Simple     ( Connection
                                                , Only(..)
                                                , query
                                                , query_
                                                )
import           Database.PostgreSQL.Simple.SqlQQ
                                                ( sql )
import           Shared.Models.Ids              ( GroupId(..) )
import           Shared.Models.Rewards.Activation
                                                ( RewardBoostActivation
                                                , activationFields
                                                )

import           Shared.Models.Rewards.Boost    ( RewardBoost
                                                , rewardFields
                                                )

getAllRewards :: Connection -> IO [RewardBoost]
getAllRewards conn = query_ conn qs
  where qs = [sql| SELECT |] <> fst rewardFields <> [sql| FROM tgthr.rewards |]

getRewardBoostActivations
  :: GroupId -> Connection -> IO [RewardBoostActivation]
getRewardBoostActivations gid conn = query conn qsActivation (Only gid)
 where
  qsActivation = [sql| SELECT |] <> fst activationFields <> [sql| 
         FROM tgthr.reward_activations 
         WHERE group_id = ?
         ORDER BY updated_at ASC 
         |]
