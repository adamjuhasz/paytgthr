module AFSM.Web.Event.ProcessGroupEventsSpec where

import           AFSM.Web.Event.ProcessGroupEvents
                                                ( activeWithin24Hrs )
import           Data.UUID                      ( nil )
import           Shared.Models.User             ( EmailAddress(EmailAddress)
                                                , Password(Password)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , UserState(UserCreated)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  describe "activeWithin24Hrs" $ do
    it "happy path" $ do
      let earlySep2  = stringToTime "2019-09-02T20:00:00+00:00"
      let september2 = stringToTime "2019-09-02T20:15:00+00:00"
      let september3 = stringToTime "2019-09-03T20:01:00+00:00"

      let user       = basicUser { usrBecameActiveOn = Just earlySep2 }

      activeWithin24Hrs september2 user `shouldBe` True
      activeWithin24Hrs september3 user `shouldBe` False

basicUser :: UserModel
basicUser = UserModel { usrFirstName          = Nothing
                      , usrLastName           = Nothing
                      , usrUserState          = UserCreated
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
