{-# LANGUAGE RecordWildCards #-}

module AFSM.Group.Change.Membership where

import           AFSM.FSM.Group                 ( GroupEvent
                                                , acceptMembership
                                                , closeGroup
                                                , denyGroup
                                                , groupStateChangeEvents
                                                , increaseGroupRevision
                                                , verifyState
                                                )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Monad.HasSaveGroupDB      ( HasSaveGroupDB(..) )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Group            ( GroupId
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

declineGroupMembership
  :: (HasGetGroupDB m, HasSaveGroupDB m)
  => TraceContext
  -> GroupId
  -> UserID
  -> m [GroupEvent]
declineGroupMembership trace groupId denier = do
  let mid = traceToMID trace

  group <- fromJust <$> getGroupByGroupId groupId
  let (events, newModel) = denyGroup denier group & increaseGroupRevision mid

  saveGroupModel newModel
  return events

approveGroupMembership
  :: (HasGetGroupDB m, HasSaveGroupDB m, MonadIO m)
  => TraceContext
  -> GroupId
  -> UserID
  -> m [GroupEvent]
approveGroupMembership trace groupId approver = do
  let mid = traceToMID trace

  tracePrint trace "ApproveGroupMembership " (approver, groupId)
  theGroup <- fromJust <$> getGroupByGroupId groupId

  let isPrimaryGroup = isNothing $ grpStart theGroup
  let involvedUsers  = mbrUser <$> grpMembers theGroup

  -- get all groups the users & partner is part of
  relatedGroups <- nub . concat <$> mapM getGroupsForUser involvedUsers
  let isActivePrimary g@GroupModel {..} acc =
        if isNothing grpStart && grpStatus == GroupActive then g : acc else acc
  let currentActivePrimaryGroups = nub $ foldr isActivePrimary [] relatedGroups

  -- accept this group
  let (acceptEvents, newModel) =
        ([], theGroup)
          & acceptMembership approver
          & verifyState
          & groupStateChangeEvents approver theGroup
          & increaseGroupRevision mid

  -- Each user can only have 1 primary group, if this group is 
  -- not the primary group, close every partner's primary group
  let closeGrp aGroup =
        ([], aGroup) & closeGroup approver & increaseGroupRevision mid
  let closedModelList = if grpStatus newModel == GroupActive && isPrimaryGroup
        then fmap closeGrp currentActivePrimaryGroups
        else []
  let closedModels = fmap snd closedModelList
  let closedEvents = concatMap fst closedModelList

  -- save all groups
  mapM_ saveGroupModel (newModel : closedModels)

  -- release messages
  return (acceptEvents <> closedEvents)

