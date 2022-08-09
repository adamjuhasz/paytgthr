module AFSM.User.Change.State where

import           AFSM.FSM.User                  ( UserEvent
                                                , adminSetState
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserDates
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.User             ( UserID
                                                , UserState
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

changeState
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> UserState
  -> m [UserEvent]
changeState trace userId newState = do
  let mid = traceToMID trace
  tracePrint trace "changeState " (userId, newState)

  now  <- getCurrentTime
  user <- fromJust <$> getUserById userId

  let (evts, newUser) =
        ([], user)
          & adminSetState newState
          & setUserDates now user
          & sendStateChangeEvents user
          & increaseUserRevision mid

  -- print debug diff
  diffUser trace user newUser

  saveUserModel newUser
  return evts
