module Scaffolding.Users where

import           Data.UUID                      ( nil )
import           Data.UUID.V4                   ( nextRandom )
import           Shared.Models.User             ( EmailAddress(..)
                                                , Password(..)
                                                , UserID(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           System.IO.Unsafe               ( unsafePerformIO )

userJohn :: UserModel
userJohn = basicUser { usrUserID       = unsafePerformIO (UserID <$> nextRandom)
                     , usrFirstName    = Just "John"
                     , usrBankVerified = Just True
                     }

userJane :: UserModel
userJane = basicUser { usrUserID       = unsafePerformIO (UserID <$> nextRandom)
                     , usrFirstName    = Just "Jane"
                     , usrBankVerified = Just True
                     }

userJulian :: UserModel
userJulian = basicUser { usrUserID    = unsafePerformIO (UserID <$> nextRandom)
                       , usrFirstName = Just "Julian"
                       }

basicUser :: UserModel
basicUser = UserModel { usrFirstName          = Just "Basic"
                      , usrLastName           = Just "User"
                      , usrUserState          = UserActive
                      , usrUserID             = UserID nil
                      , usrEmail              = EmailAddress "A@b.c"
                      , usrPassword           = Just $ Password "password"
                      , usrAddressStreet      = Nothing
                      , usrAddressStreet2     = Nothing
                      , usrAddressCity        = Nothing
                      , usrAddressState       = Nothing
                      , usrAddressZip         = Nothing
                      , usrBankRouting        = Nothing
                      , usrBankAcount         = Nothing
                      , usrBankType           = Nothing
                      , usrRevision           = 1
                      , usrVersion            = "1.0"
                      , usrPhone              = Nothing
                      , usrDOB                = Nothing
                      , usrSSN                = Nothing
                      , usrDislcosureOk       = Nothing
                      , usrConstentOk         = Nothing
                      , usrAptoCardholderID   = Nothing
                      , usrAptoKYCStatus      = Nothing
                      , usrAptoCardId         = Nothing
                      , usrAptoCardStatus     = Nothing
                      , usrMsgSource          = MessageID nil
                      , usrBankAccountName    = Nothing
                      , usrBankName           = Nothing
                      , usrDwollaId           = Nothing
                      , usrDwollaFundingId    = Nothing
                      , usrBankVerifedAmounts = Nothing
                      , usrBankVerified       = Nothing
                      , usrCreatedOn = stringToTime "2019-09-02T20:15:32+00:00"
                      , usrFirstSignIn        = Nothing
                      , usrBecameActiveOn     = Nothing
                      , usrCardCreatedOn      = Nothing
                      , usrCardActivatedOn    = Nothing
                      , usrPrivacyAcctToken   = Nothing
                      , usrEmailVerified      = False
                      , usrPhoneVerified      = False
                      }

