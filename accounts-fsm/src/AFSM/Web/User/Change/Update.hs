{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module AFSM.Web.User.Change.Update
  ( updateUser
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import qualified AFSM.User.Change.PII          as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.User             ( UserID )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.AccountsFSM.API  ( UpdateUserBody(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

updateUser
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> UpdateUserBody
  -> m NoContent
updateUser trace userId UpdateUserBody {..} = do
  evts <- retryFn trace "AFSM.changeUserPII"
    $ AFSM.changeUserPII trace userId changes

  tracePrint trace "changeUserPII casued " (userId, changes, evts)

  mapM_ (processUserEvent trace) evts

  return NoContent
