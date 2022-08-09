module AFSM.User.Change.PII
  ( changeUserPII
  ) where

import           AFSM.FSM.User                  ( UserEvent
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserDates
                                                , setUserState
                                                , updateAccount
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.User             ( UserID
                                                , UserTrait
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

changeUserPII
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> [(UserTrait, Maybe Text)]
  -> m [UserEvent]
changeUserPII trace uid changes = do
  let mid = traceToMID trace
  tracePrint trace "changeUserPII " (uid, changes)

  now   <- getCurrentTime
  model <- getUserById uid <&> fromJust

  let (events, newModel) =
        ([], model)
          & updateAccount mid changes
          & setUserState
          & setUserDates now model
          & sendStateChangeEvents model
          & increaseUserRevision mid

  -- print debug diff
  diffUser trace model newModel

  saveUserModel newModel

  tracePrint trace "changeUserPII done " (uid, changes, events)

  return events
