{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AFSM.Web.User.Change.FundingSource
  ( removeFundingSource
  , createNewFundingSource
  , verifyCurrentFS
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.FSM.User                  ( ABARouting(ABARouting)
                                                , AccountName(AccountName)
                                                , BankName(BankName)
                                                , DDANumber(DDANumber)
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(..) )
import qualified AFSM.User.Change.FundingSource
                                               as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Servant                        ( NoContent(NoContent)
                                                , ServerError(errBody)
                                                , err403
                                                , err404
                                                , err409
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.User             ( RedactedText(RedactedText)
                                                , UserID
                                                , UserModel
                                                  ( usrBankAcount
                                                  , usrBankRouting
                                                  , usrDwollaFundingId
                                                  )
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.AccountsFSM.API  ( RemoveBankFSBody(..)
                                                , TraceContext
                                                , UserCreateFSBody(..)
                                                )

removeFundingSource
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> RemoveBankFSBody
  -> m NoContent
removeFundingSource trace userid RemoveBankFSBody {..} = do
  evts <- retryFn trace "AFSM.removeFundingSource"
    $ AFSM.removeFundingSource trace userid achReason
  mapM_ (processUserEvent trace) evts
  return NoContent

createNewFundingSource
  :: (CanProcessUserEvents m, MonadError ServerError m)
  => TraceContext
  -> UserID
  -> UserCreateFSBody
  -> m NoContent
createNewFundingSource trace uid UserCreateFSBody {..} = do
  userM <- getUserById uid
  user  <- case userM of
    Nothing -> do
      traceError trace "Error: createNewFundingSource user does not exist " uid
      throwError err404
    Just u -> return u

  when
    (  (usrBankRouting user == Just (RedactedText achRouting))
    && (usrBankAcount user == Just (RedactedText achAccount))
    )
    (do
      traceError trace
                 "createNewFundingSource User tried to link current account "
                 (uid, achRouting, achAccount, user)
      throwError err409 { errBody = "User tried to link current account" }
    )
  tracePrint trace
             "createNewFundingSource "
             (uid, bankName, accountName, achRouting, achAccount)

  let debugInfo =
        ( uid
        , bankName
        , accountName
        , achRouting
        , achAccount
        , verified
        , verificationAmounts
        , accountType
        )

  existing <- case (achRouting, achAccount) of
    ("0000", "0000") -> do
      traceError trace "Error: Corporate account being linked " debugInfo
      return [] -- allow everyone to connect corporate Chime account
    _ -> getUserByBank (RedactedText achRouting) (RedactedText achAccount)
  tracePrint trace
             "createNewFundingSource getUserByBank "
             (uid, achRouting, achAccount, existing)

  case existing of
    []   -> return ()
    prev -> do
      traceError trace
                 "User tried to link existing account "
                 (uid, achRouting, achAccount, prev)
      throwError err403 { errBody = "User tried to link existing account" }

  removeEvts <- case usrDwollaFundingId user of
    Nothing -> return []
    Just _  -> do
      tracePrint trace
                 "createNewFundingSource removing old account "
                 (uid, usrDwollaFundingId user)
      AFSM.removeFundingSource trace uid Nothing

  mapM_ (processUserEvent trace) removeEvts

  evts <- retryFn trace "AFSM.addNewBankAccount" $ AFSM.addNewBankAccount
    trace
    uid
    (ABARouting achRouting)
    (DDANumber achAccount)
    (AccountName accountName)
    (BankName bankName)
    accountType
    verificationAmounts
    verified

  mapM_ (processUserEvent trace) evts

  return NoContent

verifyCurrentFS
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m NoContent
verifyCurrentFS trace uid = do
  evts <- retryFn trace "AFSM.verifyBankAccount"
    $ AFSM.verifyBankAccount trace uid

  mapM_ (processUserEvent trace) evts

  return NoContent
