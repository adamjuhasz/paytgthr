{- HLINT ignore "Reduce duplication" -}

module AFSM.User.Change.Email where

import           AFSM.FSM.User                 as FSM
                                                ( UserEvent
                                                , changeEmail
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserState
                                                , verifyEmailAddresss
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Base             ( EmailAddress )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

changeEmail
  :: (HasGetUserDB m, HasSaveUserDB m, MonadIO m)
  => TraceContext
  -> UserID
  -> EmailAddress
  -> m [UserEvent]
changeEmail trace userId newEmail = do
  let mid = traceToMID trace
  tracePrint trace "ChangeEmail " (userId, newEmail)

  model <- fromJust <$> getUserById userId

  let (evts, newModel) =
        model
          & FSM.changeEmail newEmail
          & setUserState
          & sendStateChangeEvents model
          & increaseUserRevision mid

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  return evts

verifyEmail
  :: (HasGetUserDB m, HasSaveUserDB m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
verifyEmail trace userId = do
  let mid = traceToMID trace
  tracePrint trace "verifyEmail " userId

  model <- fromJust <$> getUserById userId

  let (evts, newModel) =
        ([], model)
          & FSM.verifyEmailAddresss
          & setUserState
          & sendStateChangeEvents model
          & increaseUserRevision mid

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  return evts
