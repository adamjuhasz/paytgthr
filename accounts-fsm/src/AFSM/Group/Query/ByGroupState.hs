module AFSM.Group.Query.ByGroupState where

import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetGroupDB       ( HasGetGroupDB(..) )
import           AFSM.Utils                     ( sortGroups )
import           Shared.Models.Group            ( GroupModel
                                                , GroupStatus(..)
                                                , groupIsActive
                                                , groupIsPending
                                                )
import           Shared.Models.User             ( UserID )

getAliveGroups
  :: (HasGetGroupDB m, GetCurrentTime m) => UserID -> m [GroupModel]
getAliveGroups user = do
  now    <- getCurrentTime
  groups <- getGroupsForUserFiltered [GroupPending, GroupActive] user

  -- check they have not expired
  let pendingOrActive g = groupIsActive now g || groupIsPending now g

  let filteredGroups = filter pendingOrActive $ sortGroups now groups

  return filteredGroups
