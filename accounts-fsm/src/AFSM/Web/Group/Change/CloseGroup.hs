{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.Group.Change.CloseGroup where

import           AFSM.AppMonad                  ( CanProcessGroupEvents )
import qualified AFSM.Group.Close              as AFSM
import           AFSM.Web.Event.ProcessGroupEvents
                                                ( processGroupEvent )
import           Servant.API                    ( NoContent(..) )
import           Shared.Models.Ids              ( GroupId )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.AccountsFSM.API  ( CloseGroupBody(..)
                                                , TraceContext
                                                )

closeGroup
  :: (CanProcessGroupEvents m)
  => TraceContext
  -> GroupId
  -> CloseGroupBody
  -> m NoContent
closeGroup trace groupId CloseGroupBody {..} = do
  events <- retryFn trace "AFSM.closeGroup"
    $ AFSM.closeGroup trace groupId closer

  mapM_ (processGroupEvent trace) events

  return NoContent
