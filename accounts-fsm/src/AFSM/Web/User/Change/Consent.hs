{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.User.Change.Consent
  ( acceptConsent
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import qualified AFSM.User.Change.Legal        as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Models.Ids              ( UserID )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

acceptConsent
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m NoContent
acceptConsent traceCtx userid = do
  evts <- retryFn traceCtx "AFSM.acceptLegalConsent"
    $ AFSM.acceptLegalConsent traceCtx userid
  mapM_ (processUserEvent traceCtx) evts
  return NoContent
