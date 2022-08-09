module AFSM.Web.User.Change.IssueCard where

import           AFSM.AppMonad                  ( CanProcessUserEvents )
import           AFSM.Web.Event.ProcessUserEvents
                                                ( createCardForUser
                                                , processUserEvent
                                                )
import           Servant                        ( NoContent(..) )
import           Shared.Models.Card             ( CardDesign )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )

issueACard
  :: (CanProcessUserEvents m)
  => TraceContext
  -> UserID
  -> CardDesign
  -> m NoContent
issueACard trace userId newDesign = do
  evts <- createCardForUser trace userId newDesign

  mapM_ (processUserEvent trace) evts

  return NoContent
