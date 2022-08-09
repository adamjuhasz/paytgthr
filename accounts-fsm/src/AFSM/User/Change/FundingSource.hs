module AFSM.User.Change.FundingSource where

import           AFSM.FSM.User                  ( ABARouting
                                                , AccountName
                                                , AccountType
                                                , BankName
                                                , DDANumber
                                                , UserEvent
                                                , addManualFunding
                                                , addNewFunding
                                                , increaseUserRevision
                                                , removeFunding
                                                , sendStateChangeEvents
                                                , setUserDates
                                                , setUserState
                                                , verifyManualFunding
                                                )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import           AFSM.Monad.HasSaveUserDB       ( HasSaveUserDB(..) )
import           AFSM.User.Tools.Diff           ( diffUser )
import           Control.Monad.Reader           ( MonadIO )
import           Data.Function                  ( (&) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Payment          ( PaymentFailureCode )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

removeFundingSource
  :: (HasGetUserDB m, HasSaveUserDB m, MonadIO m)
  => TraceContext
  -> UserID
  -> Maybe PaymentFailureCode
  -> m [UserEvent]
removeFundingSource trace userId reason = do
  let mid = traceToMID trace
  tracePrint trace "RemoveFundingSource " (userId, reason)

  model <- fromJust <$> getUserById userId

  let (events, newModel) =
        removeFunding reason model & increaseUserRevision mid

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  tracePrint trace "RemoveFundingSource " (userId, events)

  return events

addNewBankAccount
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> ABARouting
  -> DDANumber
  -> AccountName
  -> BankName
  -> AccountType
  -> [Currency]
  -> Bool
  -> m [UserEvent]
addNewBankAccount trace userId routingNum accountNum accountName bankName accountType veriAmounts isVerified
  = do
    let mid = traceToMID trace
    tracePrint
      trace
      "addNewBankAccount "
      (userId, routingNum, accountType, accountName, bankName, isVerified)

    now    <- getCurrentTime
    modelM <- getUserById userId

    model  <- case modelM of
      Nothing -> do
        traceError trace "Error: User not found " userId
        error $ "Error: User not found " <> show userId
      Just m -> return m

    let (events, newModel) =
          model
            & addNewFunding routingNum
                            accountNum
                            accountName
                            bankName
                            accountType
                            veriAmounts
                            isVerified
            & setUserState
            & setUserDates now model
            & sendStateChangeEvents model
            & increaseUserRevision mid

    saveUserModel newModel

    -- print debug diff
    diffUser trace model newModel

    tracePrint trace "addNewBankAccount done " (userId, events)

    return events

addUnverifiedBankAccount
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> AccountName
  -> ABARouting
  -> DDANumber
  -> [Double]
  -> m [UserEvent]
addUnverifiedBankAccount trace userId accountName routingNum accountNum verificationAmounts
  = do
    let mid = traceToMID trace
    tracePrint trace
               "addUnverifiedBankAccount "
               (userId, routingNum, accountNum)

    now   <- getCurrentTime
    model <- fromJust <$> getUserById userId

    let (events, newModel) =
          model
            & addManualFunding routingNum
                               accountNum
                               accountName
                               verificationAmounts
            & setUserState
            & setUserDates now model
            & sendStateChangeEvents model
            & increaseUserRevision mid

    saveUserModel newModel

    -- print debug diff
    diffUser trace model newModel

    tracePrint trace "addUnverifiedBankAccount done " (userId, events)

    return events

verifyBankAccount
  :: (HasGetUserDB m, HasSaveUserDB m, GetCurrentTime m, MonadIO m)
  => TraceContext
  -> UserID
  -> m [UserEvent]
verifyBankAccount trace userId = do
  let mid = traceToMID trace
  tracePrint trace "verifyBankAccount " userId

  now   <- getCurrentTime
  model <- fromJust <$> getUserById userId

  let (events, newModel) =
        model
          & verifyManualFunding True
          & setUserState
          & setUserDates now model
          & sendStateChangeEvents model
          & increaseUserRevision mid

  saveUserModel newModel

  -- print debug diff
  diffUser trace model newModel

  tracePrint trace "verifyBankAccount done " (userId, events)

  return events
