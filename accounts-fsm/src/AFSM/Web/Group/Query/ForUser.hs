{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.Group.Query.ForUser where

import           AFSM.Group.Query.General       ( getGroupsForUser )
import           AFSM.IO.Time                   ( GetCurrentTime )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupStatus
                                                )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

getUsersGroups
  :: (HasGetGroupDB m, GetCurrentTime m)
  => TraceContext
  -> UserID
  -> [GroupStatus]
  -> m [GroupModel]
getUsersGroups _ userId desiredStates = do
  groups <- getGroupsForUser userId
  let filtered =
        filter (\GroupModel {..} -> grpStatus `elem` desiredStates) groups
  return filtered
