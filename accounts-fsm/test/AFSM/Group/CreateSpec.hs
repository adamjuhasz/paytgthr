{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Group.CreateSpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.Group                 ( GroupEvent(EventGroupCreated)
                                                )
import           AFSM.FSM.User                  ( UserEvent(EventUserCreated) )
import           AFSM.Group.Create              ( InviteInfo(..)
                                                , InvtingError(..)
                                                , createNewGroup
                                                )
import           Control.Exception              ( catch )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupMember(GroupMember)
                                                , GroupModel(..)
                                                , GroupSplit(GroupSplit)
                                                , GroupStatus(GroupPending)
                                                )
import           Shared.Models.User             ( EmailAddress(EmailAddress)
                                                , UserID(UserID)
                                                , UserModel(..)
                                                , defaultUser
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( TraceContext(TraceContext)
                                                , randomTrace
                                                )
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

      describe "special invites" $ do
            let now = stringToTime "2019-09-02T20:15:32+00:00"
            let cGetAccountByEmail e
                      | e == EmailAddress "a@example.com" = return
                            (Just $ defaultUser now e)
                      | otherwise = error $ "email unknown " <> show e
            let cGetGroupsByUserIdFiltered _ u
                      | u == UserID nil = return []
                      | otherwise       = error $ "unknown userid " <> show u
            let cSaveGroup _ = error "unknown group"
            let cSaveUser _ = return ()
            let newGid  = GroupId nil
            let inviter = UserID nil

            it "Can't invite yourself" $ do
                  trace <- randomTrace
                  let fn = createNewGroup
                            trace
                            newGid
                            inviter
                            [ ("Adan", EmailAddress "a@example.com", UserID nil)
                            , ("Bob" , EmailAddress "a@example.com", UserID nil)
                            ]
                  res <-
                        (Right <$> runReaderT
                                    (unAppIOM fn)
                                    (appConf $ dbActionsRcrd
                                          { cGetAccountByEmail
                                          , cGetGroupsByUserIdFiltered
                                          , cSaveGroup
                                          , cSaveUser
                                          }
                                    )
                              )
                              `catch` (return . Left)

                  res `shouldBe` Left CantInviteYourself

            it "Can't invite noone" $ do
                  let fn = createNewGroup (TraceContext "" 1) newGid inviter []

                  res :: Either
                              InvtingError
                              ([UserEvent], [GroupEvent], [InviteInfo], GroupId) <-
                        (Right <$> runReaderT
                                    (unAppIOM fn)
                                    (appConf $ dbActionsRcrd
                                          { cGetAccountByEmail
                                          , cGetGroupsByUserIdFiltered
                                          , cSaveGroup
                                          , cSaveUser
                                          }
                                    )
                              )
                              `catch` (return . Left)

                  res `shouldBe` Left GroupCountTooSmall

      describe "happy path" $ do
            let now    = stringToTime "2019-09-02T20:15:32+00:00"
            let newGid = GroupId nil
            let
                  cGetAccountByEmail e
                        | e == EmailAddress "a@example.com" = return
                        . Just
                        $ (defaultUser now e) { usrUserID = adanId }
                        | e == EmailAddress "b@example.com" = return
                        . Just
                        $ (defaultUser now e) { usrUserID = bobID }
                        | otherwise = error $ "email unknown " <> show e
            let cGetGroupsByUserIdFiltered _ u
                      | u == adanId = return []
                      | u == bobID  = return []
                      | otherwise   = error $ "unknown userid " <> show u
            let cSaveGroup g
                      | grpId g == newGid
                      = return ()
                      | otherwise
                      = error $ "unknown group to save " <> show g
            let cSaveUser _ = return ()
            let inviter = UserID nil

            it "works" $ do
                  trace <- randomTrace
                  let fn = createNewGroup
                            trace
                            newGid
                            inviter
                            [ ("Adan", EmailAddress "a@example.com", adanId)
                            , ("Bob" , EmailAddress "b@example.com", bobID)
                            ]

                  res :: Either
                              InvtingError
                              ([UserEvent], [GroupEvent], [InviteInfo], GroupId) <-
                        (Right <$> runReaderT
                                    (unAppIOM fn)
                                    (appConf $ dbActionsRcrd
                                          { cGetAccountByEmail
                                          , cGetGroupsByUserIdFiltered
                                          , cSaveGroup
                                          , cSaveUser
                                          }
                                    )
                              )
                              `catch` (return . Left)

                  res `shouldBe` Right
                        ( []
                        , [EventGroupCreated newGid inviter [bobID, adanId]]
                        , [ InviteInfo inviter
                                       adanId
                                       (EmailAddress "a@example.com")
                                       ""
                                       newGid
                          , InviteInfo inviter
                                       bobID
                                       (EmailAddress "b@example.com")
                                       ""
                                       newGid
                          ]
                        , newGid
                        )


            it "works if group exists" $ do
                  let
                        tempGroup = GroupModel
                              { grpId        = newGid
                              , grpStatus    = GroupPending
                              , grpStart     = Nothing
                              , grpEnd       = Nothing
                              , grpSplit     = [ GroupSplit adanId 50 True
                                               , GroupSplit bobID  50 True
                                               ]
                              , grpMembers   = [ GroupMember adanId False
                                               , GroupMember bobID  True
                                               ]
                              , grpRevision  = 1
                              , grpVersion   = "1.0"
                              , grpMsgSource = MessageID nil
                              , grpCreatedAt = unsafePerformIO getCurrentTime
                              }
                  let cGetGroupsByUserIdFilteredExisting s u
                            | u == adanId = return [tempGroup]
                            | u == bobID  = return [tempGroup]
                            | otherwise   = cGetGroupsByUserIdFiltered s u
                  trace <- randomTrace
                  let fn = createNewGroup
                            trace
                            newGid
                            inviter
                            [ ("Adan", EmailAddress "a@example.com", adanId)
                            , ("Bob" , EmailAddress "b@example.com", bobID)
                            ]

                  res :: Either
                              InvtingError
                              ([UserEvent], [GroupEvent], [InviteInfo], GroupId) <-
                        (Right <$> runReaderT
                                    (unAppIOM fn)
                                    (appConf $ dbActionsRcrd
                                          { cGetAccountByEmail
                                          , cGetGroupsByUserIdFiltered =
                                                cGetGroupsByUserIdFilteredExisting
                                          , cSaveGroup
                                          , cSaveUser
                                          }
                                    )
                              )
                              `catch` (return . Left)

                  res `shouldBe` Right
                        ( []
                        , []
                        , [ InviteInfo { inviterUserId    = UserID nil
                                       , invitedUserId    = adanId
                                       , invitedEmail = EmailAddress "a@example.com"
                                       , invitedFirstName = ""
                                       , newGroupId       = GroupId nil
                                       }
                          , InviteInfo { inviterUserId    = UserID nil
                                       , invitedUserId    = bobID
                                       , invitedEmail = EmailAddress "b@example.com"
                                       , invitedFirstName = ""
                                       , newGroupId       = GroupId nil
                                       }
                          ]
                        , GroupId nil
                        )

            it "works if user doesn't exist" $ do
                  let cGetAccountByEmailNotExist e
                            | e == EmailAddress "d@example.com" = return Nothing
                            | otherwise                   = cGetAccountByEmail e
                  let cGetGroupsByUserIdNotExist _ _ = return []
                  let cSaveUserNotExist u
                            | (usrEmail u == EmailAddress "d@example.com")
                                  && (usrFirstName u == Just "Danny")
                                  && (usrUserID u == dannyId)
                            = return ()
                            | otherwise
                            = cSaveUser u
                  trace <- randomTrace
                  let fn = createNewGroup
                            trace
                            newGid
                            inviter
                            [ ("Adan" , EmailAddress "a@example.com", adanId)
                            , ("Danny", EmailAddress "d@example.com", dannyId)
                            ]

                  res :: Either
                              InvtingError
                              ([UserEvent], [GroupEvent], [InviteInfo], GroupId) <-
                        (Right <$> runReaderT
                                    (unAppIOM fn)
                                    (appConf $ dbActionsRcrd
                                          { cGetAccountByEmail         =
                                                cGetAccountByEmailNotExist
                                          , cGetGroupsByUserIdFiltered =
                                                cGetGroupsByUserIdNotExist
                                          , cSaveGroup
                                          , cSaveUser = cSaveUserNotExist
                                          }
                                    )
                              )
                              `catch` (return . Left)

                  res `shouldBe` Right
                        ( [EventUserCreated dannyId]
                        , [EventGroupCreated newGid inviter [adanId, dannyId]]
                        , [ InviteInfo { inviterUserId    = inviter
                                       , invitedUserId    = adanId
                                       , invitedEmail = EmailAddress "a@example.com"
                                       , invitedFirstName = ""
                                       , newGroupId       = newGid
                                       }
                          , InviteInfo { inviterUserId    = inviter
                                       , invitedUserId    = dannyId
                                       , invitedEmail = EmailAddress "d@example.com"
                                       , invitedFirstName = "Danny"
                                       , newGroupId       = newGid
                                       }
                          ]
                        , newGid
                        )
