{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Redundant do" -}

module AFSM.Group.Change.MembershipSpec where

import           AFSM.AppMonad                  ( AppIOM(unAppIOM) )
import           AFSM.DB.DBActions              ( DBActions(..) )
import           AFSM.FSM.Group                 ( GroupEvent(..) )
import           AFSM.Group.Change.Membership   ( approveGroupMembership )
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
                                                , GroupStatus(..)
                                                )
import           Shared.Models.User             ( UserID(UserID) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
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
      let dbActionsRcrd = DBActions
                { cSaveUser                  = \_ -> error "mocked cSaveUser"
                , cGetAccountByEmail         = \_ -> error "mocked"
                , cGetAccountByPhone         = \_ -> error "mocked"
                , cGetAccountById = \_ -> error "mocked cGetAccountById"
                , cGetAccounByCardholder     = \_ -> error "mocked"
                , cSaveGroup                 = \_ -> error "mocked"
                , cGetGroupById              = \_ -> error "mocked"
                , cGetGroupsByUserId         = \_ -> error "mocked"
                , cGetGroupsByUserIdFiltered = \_ _ -> error "mocked"
                , cGetAllActiveUsers         = return []
                , cGetUsersWithAcountNumber  = \_ -> error "mocked"
                , cGetUsersWithSSN           = \_ -> error "mocked"
                , cFindToken                 = \_ -> error "mocked"
                , cSaveToken                 = \_ -> error "mocked"
                }

      describe "happy path" $ do
            let
                  user1 = (UserID . fromJust . fromText)
                        "00000000-0000-0000-0000-000000000000"
            let
                  user2 = (UserID . fromJust . fromText)
                        "00000000-0000-0000-0000-000000000000"
            let
                  user3 = (UserID . fromJust . fromText)
                        "00000000-0000-0000-0000-000000000000"
            let theGroupId = (GroupId . fromJust . fromText)
                      "00000000-0000-0000-0000-000000000000"
            let oldPendingGid = (GroupId . fromJust . fromText)
                      "00000000-0000-0000-0000-000000000000"
            let oldActiveGid = (GroupId . fromJust . fromText)
                      "00000000-0000-0000-0000-000000000000"
            let
                  theGroup = GroupModel
                        { grpId        = theGroupId
                        , grpStatus    = GroupPending
                        , grpStart     = Nothing
                        , grpEnd       = Nothing
                        , grpSplit     = [ GroupSplit user1 50 True
                                         , GroupSplit user2 50 True
                                         ]
                        , grpMembers   = [ GroupMember user1 False
                                         , GroupMember user2 True
                                         ]
                        , grpRevision  = 1
                        , grpVersion   = "1.0"
                        , grpMsgSource = MessageID nil
                        , grpCreatedAt = unsafePerformIO getCurrentTime
                        }
            let
                  oldPendingGroup = GroupModel
                        { grpId        = oldPendingGid
                        , grpStatus    = GroupPending
                        , grpStart     = Nothing
                        , grpEnd       = Nothing
                        , grpSplit     = [ GroupSplit user3 50 True
                                         , GroupSplit user2 50 True
                                         ]
                        , grpMembers   = [ GroupMember user3 False
                                         , GroupMember user2 True
                                         ]
                        , grpRevision  = 1
                        , grpVersion   = "1.0"
                        , grpMsgSource = MessageID nil
                        , grpCreatedAt = unsafePerformIO getCurrentTime
                        }
            let
                  oldActiveGroup = GroupModel
                        { grpId        = oldActiveGid
                        , grpStatus    = GroupActive
                        , grpStart     = Nothing
                        , grpEnd       = Nothing
                        , grpSplit     = [ GroupSplit user3 50 True
                                         , GroupSplit user2 50 True
                                         ]
                        , grpMembers   = [ GroupMember user3 True
                                         , GroupMember user2 True
                                         ]
                        , grpRevision  = 1
                        , grpVersion   = "1.0"
                        , grpMsgSource = MessageID nil
                        , grpCreatedAt = unsafePerformIO getCurrentTime
                        }
            let cGetGroupsByUserId u
                      | u == user1 = return []
                      | u == user2 = return [oldPendingGroup, oldActiveGroup]
                      | otherwise  = error $ "unknown user " <> show u
            let
                  cSaveGroup g
                        | g == theGroup { grpStatus   = GroupActive
                                        , grpRevision = 2
                                        }
                        = return ()
                        | g == oldActiveGroup { grpStatus   = GroupClosed
                                              , grpRevision = 2
                                              }
                        = return ()
                        | otherwise
                        = error $ "unknown group " <> show g
            let cGetGroupById u | u == theGroupId = return $ Just theGroup
                                | otherwise       = return Nothing

            it "works for a primary group" $ do
                  trace <- randomTrace
                  let fn = approveGroupMembership trace theGroupId user1
                  res <- runReaderT
                        (unAppIOM fn)
                        (appConf $ dbActionsRcrd { cGetGroupsByUserId
                                                 , cSaveGroup
                                                 , cGetGroupById
                                                 }
                        )

                  res
                        `shouldBe` [ EventGroupInviteAccepted
                                         (grpId theGroup)
                                         user1
                                         [user1, user2]
                                   , EventGroupStateChanged
                                         (grpId theGroup)
                                         GroupActive
                                         user1
                                         [user1, user2]
                                   , EventGroupStateChanged oldActiveGid
                                                            GroupClosed
                                                            user1
                                                            [user3, user2]
                                   ]

            it "works for a temp group" $ do
                  let   tempGroup = theGroup
                              { grpStart = Just
                                    $ stringToTime "2019-10-01T20:15:32+00:00"
                              , grpEnd   = Just
                                    $ stringToTime "2019-11-01T20:15:32+00:00"
                              }
                        tempCSaveGroup g
                              | g == tempGroup { grpStatus   = GroupActive
                                               , grpRevision = 2
                                               }
                              = return ()
                              | otherwise
                              = cSaveGroup g
                  trace <- randomTrace
                  let fn = approveGroupMembership trace theGroupId user1
                  res <- runReaderT
                        (unAppIOM fn)
                        (appConf $ dbActionsRcrd { cGetGroupsByUserId
                                                 , cGetGroupById
                                                 , cSaveGroup = tempCSaveGroup
                                                 }
                        )

                  res
                        `shouldBe` [ EventGroupInviteAccepted
                                         (grpId tempGroup)
                                         user1
                                         [user1, user2]
                                   , EventGroupStateChanged
                                         (grpId tempGroup)
                                         GroupActive
                                         user1
                                         [user1, user2]
                                   , EventGroupStateChanged oldActiveGid
                                                            GroupClosed
                                                            user1
                                                            [user3, user2]
                                   ]
