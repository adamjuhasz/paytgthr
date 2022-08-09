module AFSM.Web.Group.Change.AcceptSplit where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import qualified AFSM.Group.Change.Split       as AFSM
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Ids              ( GroupId
                                                , UserID
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

acceptSplit
  :: (CanProcessGroupEvents m)
  => TraceContext
  -> GroupId
  -> UserID
  -> m NoContent
acceptSplit trace groupId acceptor = do
  events <- retryFn trace "AFSM.approveGroupSplit"
    $ AFSM.approveGroupSplit trace groupId acceptor

  mapM_ (processGroupEvent trace) events

  return NoContent
