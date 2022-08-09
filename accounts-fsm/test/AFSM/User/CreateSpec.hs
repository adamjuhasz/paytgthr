{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use &&" -}

module AFSM.User.CreateSpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.User                  ( UserEvent
                                                  ( EventUserCreated
                                                  , EventUserStateChanged
                                                  )
                                                )
import           AFSM.User.Create               ( createUser )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Maybe                     ( fromJust )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , Password(..)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
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
        , cGetAccountById            = \_ -> error "mocked cGetAccountById"
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

  describe "Creates a User" $ do
    it "does not exist before" $ do
      let idempotent =
            UserID
              . fromJust
              . fromText
              $ "00000000-0000-0000-0000-000000000000"
      let securePassword = Just $ Password "password"
      let saveUser u
            | and
              [ usrUserID u == idempotent
              , usrPassword u == securePassword
              , usrRevision u == 1
              ]
            = return ()
            | otherwise
            = error "unkbown user"
      let getUser e | e == EmailAddress "me@example.com" = return Nothing
                    | otherwise                          = error "bad user id"
      let db = dbActionsRcrd { cGetAccountByEmail = getUser
                             , cSaveUser          = saveUser
                             }
      let fn = createUser (MessageID nil)
                          (EmailAddress "me@example.com")
                          securePassword
                          Nothing
                          idempotent

      res <- runReaderT (unAppIOM fn) (appConf db)
      res `shouldBe` (idempotent, [EventUserCreated idempotent])

    it "has a ghost profile" $ do
      let idempotent =
            UserID
              . fromJust
              . fromText
              $ "00000000-0000-0000-0000-000000000000"
      let securePassword = Just $ Password "password"
      let saveUser u
            | and
              [ usrUserID u == usrUserID basicUser
              , usrPassword u == securePassword
              , usrRevision u == 2
              ]
            = return ()
            | otherwise
            = error "unkbown user"
      let getUser e
            | e == EmailAddress "me@example.com" = return $ Just basicUser
            | otherwise                          = error "bad user id"
      let db = dbActionsRcrd { cGetAccountByEmail = getUser
                             , cSaveUser          = saveUser
                             }
      let fn = createUser (MessageID nil)
                          (EmailAddress "me@example.com")
                          securePassword
                          Nothing
                          idempotent

      res <- runReaderT (unAppIOM fn) (appConf db)
      res
        `shouldBe` ( usrUserID basicUser
                   , [ EventUserStateChanged (usrUserID basicUser)
                                             UserWaitingOnPII
                     ]
                   )

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
