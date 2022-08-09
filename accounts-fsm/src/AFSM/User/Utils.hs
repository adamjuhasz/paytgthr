module AFSM.User.Utils where

import           AFSM.FSM.User                  ( UserEvent(..) )
import           Shared.TgthrMessages.Accounts  ( AccountsEvent(..) )

mapUserEventsAccountEvents :: UserEvent -> [AccountsEvent]
mapUserEventsAccountEvents (EventUserCreated uid) = [UserWasCreated uid]
mapUserEventsAccountEvents (EventUserStateChanged userId newState) =
  [UserStateChange userId newState]
mapUserEventsAccountEvents (EventUserInfoChanged userId state changes) =
  [UserWasUpdated userId state changes]
mapUserEventsAccountEvents (EventUserFSRemoved userId bankInfo reason) =
  [FundingSourceRemoved userId bankInfo reason]
-- No match
mapUserEventsAccountEvents EventUserFSAdded{}                = []
mapUserEventsAccountEvents EventUserFSVerified{}             = []
mapUserEventsAccountEvents EventUserPasswordChanged{}        = []
mapUserEventsAccountEvents EventUserEmailChanged{}           = []
mapUserEventsAccountEvents EventUserCardStateChangedFromTo{} = []
mapUserEventsAccountEvents EventUserKYCStateChangedFromTo{}  = []
mapUserEventsAccountEvents EventUserEmailVerified{}          = []
mapUserEventsAccountEvents EventUserPhoneVerified{}          = []
mapUserEventsAccountEvents EventUserCardCreated{}            = []
