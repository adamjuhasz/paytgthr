module AFSM.IO.Random where

import           Data.UUID                      ( UUID )
import           Shared.Models.Invite           ( InviteCode )
import           Shared.Models.Referral.ReferralCode
                                                ( ReferralCodeDisplay )

class Monad m => HasRandom m where
  getUUID                   :: m UUID
  getRandomInviteCode       :: m InviteCode
  getRandomReferralCode     :: m ReferralCodeDisplay
