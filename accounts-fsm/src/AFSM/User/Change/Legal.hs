module AFSM.User.Change.Legal
  ( acceptDisclosures
  , acceptLegalConsent
  ) where

import           AFSM.FSM.User                  ( UserEvent
                                                , acceptConsent
                                                , acceptDisclosure
                                                , increaseUserRevision
                                                , sendStateChangeEvents
                                                , setUserDates
                                                , setUserState
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

acceptDisclosures
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
acceptDisclosures trace userId = do
  let mid = traceToMID trace
  tracePrint trace "AcceptDisclosure " userId

  model <- fromJust <$> getUserById userId
  now   <- getCurrentTime

  let (evts, newModel) =
        ( increaseUserRevision mid
          . sendStateChangeEvents model
          . setUserDates now model
          . setUserState
          . acceptDisclosure now mid
          )
          model

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  return evts


acceptLegalConsent
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
acceptLegalConsent trace userId = do
  let mid = traceToMID trace
  tracePrint trace "AcceptConsent " userId

  model <- fromJust <$> getUserById userId
  now   <- getCurrentTime

  let (events, newModel) =
        ( increaseUserRevision mid
          . sendStateChangeEvents model
          . setUserDates now model
          . setUserState
          . acceptConsent now mid
          )
          model

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  return events
