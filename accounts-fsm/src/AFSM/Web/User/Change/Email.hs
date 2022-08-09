{-# LANGUAGE FlexibleContexts #-}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Web.User.Change.Email
  ( changeEmail
  , verifyEmail
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import qualified AFSM.User.Change.Email        as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Models.Base             ( EmailAddress
                                                , UserID
                                                )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

changeEmail
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> EmailAddress
  -> m NoContent
changeEmail trace userid newEmail = do
  evts <- retryFn trace "AFSM.changeEmail"
    $ AFSM.changeEmail trace userid newEmail

  mapM_ (processUserEvent trace) evts

  return NoContent

verifyEmail
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m NoContent
verifyEmail trace userid = do
  evts <- retryFn trace "AFSM.verifyEmail" $ AFSM.verifyEmail trace userid

  mapM_ (processUserEvent trace) evts

  return NoContent
