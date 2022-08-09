{-# LANGUAGE FlexibleContexts #-}

module AFSM.Web.User.Change.Disclosure
  ( acceptDisclosure
  ) where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import qualified AFSM.User.Change.Legal        as AFSM
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Models.Ids              ( UserID )
import           Shared.Utils.Retry             ( retryFn )
import           Shared.WebAPI.General.API      ( TraceContext )

acceptDisclosure
  :: (CanProcessUserEvents m) => TraceContext -> UserID -> m NoContent
acceptDisclosure trace userid = do
  evts <- retryFn trace "AFSM.acceptDisclosures"
    $ AFSM.acceptDisclosures trace userid
  mapM_ (processUserEvent trace) evts
  return NoContent
