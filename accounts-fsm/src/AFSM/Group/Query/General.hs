module AFSM.Group.Query.General where

import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetGroupDB      as GG
                                                ( HasGetGroupDB(..) )
import           AFSM.Utils                     ( sortGroups )
import           Shared.Models.Group            ( GroupId
                                                , GroupModel
                                                )
import           Shared.Models.User             ( UserID )

getGroup :: HasGetGroupDB m => GroupId -> m (Maybe GroupModel)
getGroup = getGroupByGroupId

getGroupsForUser
  :: (HasGetGroupDB m, GetCurrentTime m) => UserID -> m [GroupModel]
getGroupsForUser user = do
  now    <- getCurrentTime
  groups <- GG.getGroupsForUser user
  return $ sortGroups now groups
