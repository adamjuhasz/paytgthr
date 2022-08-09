{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use &&" -}

module AFSM.User.Change.PasswordSpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.User                  ( UserEvent
                                                      ( EventUserPasswordChanged
                                                      )
                                                )
import           AFSM.User.Change.Password      ( changePassword )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.UUID                      ( nil )
import           Shared.Models.User             ( EmailAddress(EmailAddress)
                                                , Password(..)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( randomTrace )
import           Test.AppConf                   ( appConf )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
      let uid = UserID nil
      let dbActionsRcrd = DBActions
                { cSaveUser                  = \_ -> error "mocked cSaveUser"
                , cGetAccountByEmail         = \_ -> error "mocked"
                , cGetAccountByPhone         = \_ -> error "mocked"
                , cGetAccountById = \_ -> error "mocked cGetAccountById"
                , cGetAccounByCardholder     = \_ -> error "mocked"
                , cSaveGroup                 = \_ -> error "mocked"
                , cGetGroupById              = \_ -> error "mocked"
                , cGetGroupsByUserId         = \_ -> error "mocked"
                , cGetGroupsByUserIdFiltered = \_ -> error "mocked"
                , cGetAllActiveUsers         = return []
                , cGetUsersWithAcountNumber  = \_ -> error "mocked"
                , cGetUsersWithSSN           = \_ -> error "mocked"
                , cFindToken                 = \_ -> error "mocked"
                , cSaveToken                 = \_ -> error "mocked"
                }

      describe "Changes the password" $ do
            it "happy path" $ do
                  let saveUser u
                            | and
                                  [ usrRevision u == 2 -- we bumped rev +1
                                  , usrPassword u == Just (Password "password")
                                  ]
                            = return ()
                            | otherwise
                            = error "unkbown user"
                  let getUser _ = return $ Just basicUser
                  let db = dbActionsRcrd { cSaveUser       = saveUser
                                         , cGetAccountById = getUser
                                         }
                  trace <- randomTrace
                  let fn = changePassword trace uid (Password "password")

                  res <- runReaderT (unAppIOM fn) (appConf db)
                  res `shouldBe` [EventUserPasswordChanged uid]

basicUser :: UserModel
basicUser = UserModel { usrFirstName          = Just "Basic"
                      , usrLastName           = Just "User"
                      , usrUserState          = UserWaitingOnPII
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
