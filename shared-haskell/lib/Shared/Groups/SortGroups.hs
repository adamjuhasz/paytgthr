module Shared.Groups.SortGroups where

import           Data.List                      ( sortBy )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.Group            ( groupHasStarted
                                                , groupIsActive
                                                , groupIsPending
                                                , GroupModel(..)
                                                )

sortGroups :: UTCTime -> [GroupModel] -> [GroupModel]
sortGroups now groups = notClosedGroups <> leftOverGroups
 where
  activeGroups  = sortBy sortByTime $ filter (groupIsActive now) groups
  pendingGroups = sortBy sortByTime $ filter (groupIsPending now) groups
  upcomingGroups =
    sortBy sortByTime $ filter (not . groupHasStarted now) groups
  notClosedGroups = activeGroups <> upcomingGroups <> pendingGroups
  leftOverGroups =
    sortBy sortByTime $ filter (`notElem` notClosedGroups) groups

sortByTime :: GroupModel -> GroupModel -> Ordering
sortByTime GroupModel { grpEnd = Just t1 } GroupModel { grpEnd = Just t2 } =
  compare t1 t2
sortByTime GroupModel { grpEnd = Nothing } GroupModel { grpEnd = Just _ }  = GT
sortByTime GroupModel { grpEnd = Just _ }  GroupModel { grpEnd = Nothing } = LT
sortByTime GroupModel { grpEnd = Nothing } GroupModel { grpEnd = Nothing } = EQ
