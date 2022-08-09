module AFSM.Monad.HasInviteCode where

import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Invite           ( InviteCode
                                                , PartnerInvite
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasInviteCode m where
  getUsersInvite :: TraceContext -> UserID        -> m (Maybe PartnerInvite)
  getInvite      :: TraceContext -> InviteCode    -> m (Maybe PartnerInvite)
  saveInvite     :: TraceContext -> PartnerInvite -> m ()
