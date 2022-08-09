{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.User.Change.KYC where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.User.Change.KYC           ( runKYC )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Shared.Models.KYCAssesment     ( KYCAssesment )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.AccountsFSM.API  ( TraceContext )

runCIPProgram
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m KYCAssesment
runCIPProgram trace userId = do
  (assessment, evts) <- runKYC trace userId

  mapM_ (processUserEvent trace) evts

  return assessment

