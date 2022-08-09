module AFSM.Monad.HasGetGroupDB where

import           Shared.Models.CategorySplit    ( CategorySplit )
import           Shared.Models.Group            ( GroupId(..)
                                                , GroupModel
                                                , GroupStatus
                                                )
import           Shared.Models.Ids              ( UserID(..) )

type AllowedGroupStatus = GroupStatus

class Monad m => HasGetGroupDB m where
  getGroupByGroupId :: GroupId -> m (Maybe GroupModel)
  getGroupsForUserFiltered :: [AllowedGroupStatus] -> UserID -> m [GroupModel]
  getGroupsForUser :: UserID -> m [GroupModel]
  getGroupMainSplit :: GroupId -> m (Maybe CategorySplit)
  getGroupSplits :: GroupId -> m [CategorySplit]
