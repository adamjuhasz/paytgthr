module AFSM.Web.User.Change.Phone where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import qualified AFSM.FSM.User                 as FSM
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

verifyPhone
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m NoContent
verifyPhone trace userid = do
  let messageId = traceToMID trace
  tracePrint trace "verifyPhone " userid

  model <- fromJust <$> getUserById userid

  let (evts, newModel) =
        ([], model)
          & FSM.verifyPhoneNumber
          & FSM.setUserState
          & FSM.sendStateChangeEvents model
          & FSM.increaseUserRevision messageId

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  mapM_ (processUserEvent trace) evts

  return NoContent
