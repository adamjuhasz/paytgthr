module AFSM.Monad.HasRewardsDB where

import           Shared.Models.Ids
import           Shared.Models.Rewards.Activation
                                                ( ActivationState
                                                , RewardBoostActivation
                                                )
import           Shared.Models.Rewards.Boost    ( RewardBoost )
import           Shared.WebAPI.General.API      ( TraceContext )

class (Monad m) => HasRewardsDB m where
  getGroupsCurrentRewards :: TraceContext -> GroupId -> m [RewardBoost]
  saveBoostActivation     :: TraceContext -> RewardBoostActivation -> m ()
  getGroupsActivations    :: TraceContext -> GroupId -> [ActivationState] -> m [RewardBoostActivation]
  getAllActiveRewards     :: TraceContext -> m [RewardBoost ]
  saveRewardBoost         :: TraceContext -> RewardBoost -> m ()
  getRewardBoost          :: TraceContext -> RewardId -> m (Maybe RewardBoost)
  getAllRewardsEver       :: TraceContext -> m [RewardBoost ]
  getBoostActivation      :: TraceContext -> ActivatedRewardId -> m (Maybe RewardBoostActivation)
