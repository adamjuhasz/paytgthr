module AFSM.User.Close where

import           AFSM.FSM.User                  ( UserEvent
                                                , closeAccount
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserDates
                                                , setUserState
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.User             ( ClosureReason
                                                , UserID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

closeUser
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> ClosureReason
  -> m [UserEvent]
closeUser trace userId reason = do
  let mid = traceToMID trace
  tracePrint trace "CloseUser " (userId, reason)

  now   <- getCurrentTime
  model <- fromJust <$> getUserById userId

  let (events, newModel) =
        model
          & closeAccount reason
          & setUserState
          & setUserDates now model
          & sendStateChangeEvents model
          & increaseUserRevision mid

  saveUserModel newModel
  return events
