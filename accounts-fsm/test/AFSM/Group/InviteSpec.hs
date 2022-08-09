{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Group.InviteSpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.Group                 ( GroupEvent(EventGroupCreated)
                                                )
import           AFSM.FSM.User                  ( UserEvent )
import           AFSM.Group.Create              ( InviteInfo(..)
                                                , InvtingError(..)
                                                )
import           AFSM.Group.Invite              ( inviteUser )
import           Control.Exception              ( catch )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Maybe                     ( fromJust )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupModel(..)
                                                )
import           Shared.Models.User             ( EmailAddress(EmailAddress)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , defaultUser
                                                )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( randomTrace )
import           Test.AppConf                   ( appConf )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

adanId :: UserID
adanId = UserID $ fromJust $ fromText "00000000-0000-0000-0000-000000000000"

bobID :: UserID
bobID = UserID $ fromJust $ fromText "00000000-0000-0000-0000-000000000000"

dannyId :: UserID
dannyId = UserID $ fromJust $ fromText "00000000-0000-0000-0000-000000000000"

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

  describe "happy path" $ do
    let now    = stringToTime "2019-09-02T20:15:32+00:00"
    let newGid = GroupId nil
    let cGetAccountByEmail e
          | e == EmailAddress "a@example.com" = return . Just $ (defaultUser now e)
            { usrUserID = adanId
            }
          | e == EmailAddress "b@example.com" = return . Just $ (defaultUser now e)
            { usrUserID = bobID
            }
          | otherwise = error $ "email unknown " <> show e
    let cGetGroupsByUserIdFiltered _ u
          | u == adanId = return []
          | u == bobID  = return []
          | otherwise   = error $ "unknown userid " <> show u
    let cSaveGroup g | grpId g == newGid = return ()
                     | otherwise = error $ "unknown group to save " <> show g
    let cSaveUser u = error $ "unknown user to save " <> show u
    let cGetAccountById u
          | u == adanId = return
          $ Just
          $ (defaultUser now (EmailAddress "a@example.com"))
              { usrFirstName = Just "Adan"
              , usrUserID    = adanId
              }
          | otherwise = return Nothing

    it "works" $ do
      trace <- randomTrace
      let fn = inviteUser trace newGid adanId "Bob" (EmailAddress "b@example.com")

      res :: Either
          InvtingError
          ([UserEvent], [GroupEvent], [InviteInfo], GroupId) <-
        (Right <$> runReaderT
            (unAppIOM fn)
            (appConf $ dbActionsRcrd { cGetAccountByEmail
                                     , cGetGroupsByUserIdFiltered
                                     , cSaveGroup
                                     , cSaveUser
                                     , cGetAccountById
                                     }
            )
          )
          `catch` (return . Left)

      res `shouldBe` Right
        ( []
        , [EventGroupCreated newGid adanId [bobID, adanId]]
        , [ InviteInfo adanId adanId (EmailAddress "a@example.com") "" newGid
          , InviteInfo adanId bobID  (EmailAddress "b@example.com") "" newGid
          ]
        , newGid
        )
