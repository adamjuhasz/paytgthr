module AFSM.Monad.HasReferralDB where

import           Shared.Models.Ids              ( ReferralProgramID
                                                , UserID
                                                )
import           Shared.Models.Referral.ReferralCode
                                                ( ReferralCode
                                                , ReferralCodeDisplay
                                                )
import           Shared.Models.Referral.ReferralProgram
                                                ( ReferralProgram )
import           Shared.Models.Referral.ReferralProgress
                                                ( ReferralProgress )
import           Shared.WebAPI.General.API      ( TraceContext )

class (Monad m) => HasReferralDB m where
  getPublicReferralProgram :: TraceContext -> m (Maybe ReferralProgram)
  saveReferralProgram      :: TraceContext -> ReferralProgram -> m ()
  getReferralProgram       :: TraceContext -> ReferralProgramID -> m (Maybe ReferralProgram)
  getReferralCodeForUID    :: TraceContext -> UserID -> m (Maybe ReferralCode)
  getReferralCode          :: TraceContext -> ReferralCodeDisplay -> m (Maybe ReferralCode)
  saveReferralCode         :: TraceContext -> ReferralCode -> m ()
  getReferralProgressFor   :: TraceContext -> UserID -> m (Maybe ReferralProgress)
  getReferreeProgressFor   :: TraceContext -> UserID -> m [ReferralProgress]
  saveReferralProgress     :: TraceContext -> ReferralProgress -> m ()
