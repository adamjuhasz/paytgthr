module AFSM.Web.Group.Change.ForceSplit where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import qualified AFSM.Group.Change.Split       as AFSM
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Group            ( GroupId
                                                , GroupSplit
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

forceSplit
  :: (CanProcessGroupEvents m)
  => TraceContext
  -> GroupId
  -> [GroupSplit]
  -> m NoContent
forceSplit trace groupId splits = do
  events <- retryFn trace "AFSM.forceSplitChange"
    $ AFSM.forceSplitChange trace groupId splits

  mapM_ (processGroupEvent trace) events

  return NoContent
