{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use &&" -}

module AFSM.User.Change.PIISpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.User                  ( UserEvent
                                                      ( EventUserInfoChanged
                                                      )
                                                )
import           AFSM.User.Change.PII           ( changeUserPII )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.UUID                      ( nil )
import           Shared.Models.User             ( EmailAddress(..)
                                                , Password(Password)
                                                , UserChanges(..)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API
import           Test.AppConf                   ( appConf )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
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

      describe "Changes user information" $ do
            it "changes 1 field" $ do
                  let saveUser u
                            | and
                                  [ usrRevision u == 2 -- we bumped rev +1
                                  , usrFirstName u == Just "Avery"
                                  ]
                            = return ()
                            | otherwise
                            = error "unkbown user"
                  let getUser _ = return $ Just basicUser
                  let db = dbActionsRcrd { cSaveUser       = saveUser
                                         , cGetAccountById = getUser
                                         }
                  let changes = [(NameFirst, Just "Avery")]
                  trace <- randomTrace
                  let fn = changeUserPII trace (usrUserID basicUser) changes

                  res <- runReaderT (unAppIOM fn) (appConf db)
                  res
                        `shouldBe` [ EventUserInfoChanged (UserID nil)
                                                          UserWaitingOnPII
                                                          [UsersName]
                                   ]

            it "changes all fields" $ do
                  let saveUser u
                            | and
                                  [ usrRevision u == 2 -- we bumped rev +1
                                  , usrFirstName u == Just "Avery"
                                  , usrUserState u == UserWaitingOnPII
                                  ]
                            = return ()
                            | otherwise
                            = error $ "unkbown user " <> show u
                  let getUser _ = return $ Just basicUser
                  let db = dbActionsRcrd { cSaveUser       = saveUser
                                         , cGetAccountById = getUser
                                         }
                  let changes =
                            [ (NameFirst    , Just "Avery")
                            , (NameLast     , Just "Smith")
                            , (AddressStreet, Just "123 Main St")
                            , (AddressCity  , Just "Irvie")
                            , (AddressState , Just "CA")
                            , (AddressZip   , Just "12345")
                            , (DateOfBirth  , Just "08/02/1985")
                            , (EncryptedSSN , Just "123-45-1234")
                            , (Phone        , Just "2341231234")
                            ]
                  trace <- randomTrace
                  let fn = changeUserPII trace (usrUserID basicUser) changes

                  res <- runReaderT (unAppIOM fn) (appConf db)
                  res
                        `shouldBe` [ EventUserInfoChanged
                                           (UserID nil)
                                           UserWaitingOnPII
                                           [ UsersName
                                           , UsersAddress
                                           , UsersPhone
                                           , UsersPII
                                           ]
                                   ]

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
