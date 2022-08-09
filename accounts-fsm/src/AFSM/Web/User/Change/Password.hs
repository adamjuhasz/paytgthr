{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.User.Change.Password
  ( changePassword
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import qualified AFSM.User.Change.Password     as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Models.User             ( Password
                                                , UserID
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

changePassword
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> Password
  -> m NoContent
changePassword traceCtx userid password = do
  evts <- retryFn traceCtx "AFSM.changePassword"
    $ AFSM.changePassword traceCtx userid password
  mapM_ (processUserEvent traceCtx) evts
  return NoContent
