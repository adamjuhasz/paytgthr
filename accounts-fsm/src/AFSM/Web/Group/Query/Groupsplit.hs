module AFSM.Web.Group.Query.Groupsplit where

import           AFSM.Monad.HasGetGroupDB               ( HasGetGroupDB(..) )
import           Shared.Models.CategorySplit    ( CategorySplit )
import           Shared.Models.Group            ( GroupId )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

getGroupCatSplits
  :: (HasGetGroupDB m) => TraceContext -> GroupId -> m [CategorySplit]
getGroupCatSplits _trace = getGroupSplits
