module AFSM.Web.Group.Change.ApproveMembership where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import qualified AFSM.Group.Change.Membership  as AFSM
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Ids              ( GroupId
                                                , UserID
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

approveGroupMembership
  :: (CanProcessGroupEvents m)
  => TraceContext
  -> GroupId
  -> UserID
  -> m NoContent
approveGroupMembership trace groupId approver = do
  events <- retryFn trace "AFSM.approveGroupMembership"
    $ AFSM.approveGroupMembership trace groupId approver

  mapM_ (processGroupEvent trace) events

  return NoContent
