module AFSM.Web.User.Change.CreateCardholder where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.User.Change.Cardholder    ( getCardHolderId )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( processUserEvent )
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.AccountsFSM.API  ( CreateCardholderAction
                                                , TraceContext
                                                )

sendToIssuer
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> m CreateCardholderAction
sendToIssuer trace userId = do
  (res, evts) <- getCardHolderId trace userId

  mapM_ (processUserEvent trace) evts

  return res
