{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.Rewards.Rewards where

import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasEventTracking    ( HasEventTracking
                                                  ( trackEventWithProps
                                                  )
                                                )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasRewardsDB        ( HasRewardsDB(..) )
import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock                ( addUTCTime )
import           Servant                        ( NoContent(NoContent) )
import           Servant.Server                 ( ServerError(errBody)
                                                , err404
                                                , err500
                                                )
import           Shared.Models.Group            ( GroupMember(..)
                                                , GroupModel
                                                  ( GroupModel
                                                  , grpMembers
                                                  )
                                                )
import           Shared.Models.Ids
import           Shared.Models.Rewards.Activation

import           Shared.Models.Rewards.Boost    ( RewardBoost(..) )
import           Shared.WebAPI.AccountsFSM.API  ( ActivateRewardBody(..)
                                                , MarkRewardUsedBody(..)
                                                , TraceContext
                                                )

getGroupsActiveRewards
  :: (HasRewardsDB m) => TraceContext -> GroupId -> m [RewardBoost]
getGroupsActiveRewards = getGroupsCurrentRewards

activateRewardForGroup
  :: (HasRewardsDB m, GetCurrentTime m, HasEventTracking m, HasGetGroupDB m)
  => TraceContext
  -> GroupId
  -> ActivateRewardBody
  -> m [RewardBoost]
activateRewardForGroup trace gid ActivateRewardBody {..} = do
  now       <- getCurrentTime

  theReward <- fromJust <$> getRewardBoost trace rewardToActivate
  let rewardLevel = boostRewardInBips theReward

  activations <- getGroupsActivations trace gid [BoostActive]
  rewards     <- mapM (\x -> (x, ) <$> getRewardBoost trace (activatedReward x))
                      activations
  let foldOutMissingBoosts (_, Nothing) accum = accum
      foldOutMissingBoosts (x, Just r) accum =
        [ (x, r) | boostRewardInBips r == rewardLevel ] <> accum

  let rewardsInLevel = foldr foldOutMissingBoosts [] rewards
  cancelled <- case rewardsInLevel of
    []           -> return Nothing
    [        _ ] -> return Nothing
    (act, r) : _ -> do
      let cancelled = act { activationState     = BoostCancelled
                          , activationChangeBy  = activatedBy
                          , activationUpdatedAt = now
                          }
      saveBoostActivation trace cancelled
      return $ Just r


  let timeToAdd = fromInteger $ boostExpiresInHr theReward * 3600

  let newActivation = RewardBoostActivation
        { activationId        = newActivationId
        , activatedReward     = rewardToActivate
        , activatedGroup      = gid
        , activationCreatedAt = now
        , activationUpdatedAt = now
        , activationChangeBy  = activatedBy
        , activationExpires   = addUTCTime timeToAdd now
        , activationState     = BoostActive
        , activationUsesLeft  = boostUses theReward
        }

  saveBoostActivation trace newActivation

  let sendEvent u = do
        trackEventWithProps u "Group RewardBoost Changed" $ object
          [ "activated" .= boostName theReward
          , "level" .= boostRewardInBips theReward
          , "id" .= boostId theReward
          , "changedBy" .= activatedBy
          , "cancelled" .= (boostName <$> cancelled)
          ]
        case cancelled of
          Nothing -> return ()
          Just rb ->
            trackEventWithProps u "Group RewardBoost Cancelled" $ object
              [ "activated" .= boostName rb
              , "level" .= boostRewardInBips rb
              , "id" .= boostId rb
              , "changedBy" .= activatedBy
              ]

  groupM <- getGroupByGroupId gid
  case groupM of
    Nothing -> sendEvent activatedBy
    Just GroupModel { grpMembers } ->
      forM_ grpMembers $ \(GroupMember u _) -> sendEvent u

  getGroupsCurrentRewards trace gid

getActiveRewards :: (HasRewardsDB m) => TraceContext -> m [RewardBoost]
getActiveRewards = getAllActiveRewards

getSpecific :: (HasRewardsDB m) => TraceContext -> RewardId -> m RewardBoost
getSpecific trace rid = do
  rewardMaybe <- getRewardBoost trace rid
  case rewardMaybe of
    Nothing -> error "Does not exist"
    Just r  -> return r

createNewRewardBoost
  :: (HasRewardsDB m, GetCurrentTime m)
  => TraceContext
  -> RewardBoost
  -> m NoContent
createNewRewardBoost trace reward = do
  now <- getCurrentTime
  let normalized = reward { boostCreated = now, boostUpdated = now }
  saveRewardBoost trace normalized
  return NoContent

getHistoricalRewards :: (HasRewardsDB m) => TraceContext -> m [RewardBoost]
getHistoricalRewards = getAllRewardsEver

updateReward
  :: (HasRewardsDB m, GetCurrentTime m)
  => TraceContext
  -> RewardId
  -> RewardBoost
  -> m NoContent
updateReward trace _ reward = do
  now <- getCurrentTime
  let checkedReward = reward { boostUpdated = now }
  saveRewardBoost trace checkedReward
  return NoContent

markAsUsed
  :: (HasRewardsDB m, GetCurrentTime m, MonadError ServerError m)
  => TraceContext
  -> GroupId
  -> RewardId
  -> MarkRewardUsedBody
  -> m NoContent
markAsUsed trace gid rid MarkRewardUsedBody{} = do
  now         <- getCurrentTime
  activations <- getGroupsActivations trace gid [BoostActive]
  let matchingActivation = filter
        (\RewardBoostActivation {..} -> activatedReward == rid)
        activations

  case matchingActivation of
    [] -> throwError $ err404 { errBody = "Cant find an activation" }
    _x : _x' : _xs ->
      throwError $ err500 { errBody = "Multiple activations found" }
    [RewardBoostActivation { activationUsesLeft = Nothing }   ] -> return ()
    [rba@RewardBoostActivation { activationUsesLeft = Just 1 }] -> do
      let updatedActivation = rba { activationState     = BoostUsed
                                  , activationUsesLeft  = Just 0
                                  , activationUpdatedAt = now
                                  }
      saveBoostActivation trace updatedActivation
    [rba@RewardBoostActivation { activationUsesLeft = Just uses }] -> do
      let updatedActivation = rba { activationUsesLeft  = Just $ uses - 1
                                  , activationUpdatedAt = now
                                  }
      saveBoostActivation trace updatedActivation

  return NoContent

rewardsCancelSpecific
  :: (HasRewardsDB m, GetCurrentTime m, MonadError ServerError m)
  => TraceContext
  -> ActivatedRewardId
  -> m NoContent
rewardsCancelSpecific trace rid = do
  now         <- getCurrentTime
  activationM <- getBoostActivation trace rid
  activation  <- case activationM of
    Nothing  -> throwError $ err404 { errBody = "Cant find an activation" }
    Just rba -> return rba

  let updatedActivation = activation { activationState     = BoostCancelled
                                     , activationUpdatedAt = now
                                     }
  saveBoostActivation trace updatedActivation

  return NoContent
