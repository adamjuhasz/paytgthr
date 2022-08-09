{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.FSMSpec
  ( spec
  ) where

import           AFSM.FSM.Group                 ( GroupEvent(..)
                                                , approveRatio
                                                , createGroup
                                                )
import           AFSM.FSM.User                  ( createAccount
                                                , updateAccount
                                                )
import           Control.Exception              ( evaluate )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupMember(..)
                                                , GroupModel(..)
                                                , GroupSplit(..)
                                                , GroupStatus(..)
                                                , verifyGroupCohesion
                                                )
import           Shared.Models.User            as USR
                                                ( EmailAddress(..)
                                                , Password(Password)
                                                , UserID(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                , UserTrait(..)
                                                , defaultUser
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , errorCall
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldSatisfy
                                                , shouldThrow
                                                )

nilU :: UserID
nilU = UserID nil

nilEmail :: EmailAddress
nilEmail = EmailAddress "test@paytgthr.dev"

nilMessage :: MessageID
nilMessage = MessageID nil

spec :: Spec
spec = parallel $ do
  let isCreated = (== USR.UserCreated) . usrUserState
      hasEmail e = (== EmailAddress e) . usrEmail

  describe "createAccount" $ do
    it "happy path" $ do
      let now = stringToTime "2019-09-02T20:15:32+00:00"
      let p   = Password "Password"
      snd (createAccount now nilMessage nilU nilEmail (Just p))
        `shouldBe` (defaultUser now nilEmail) { usrUserID   = nilU
                                              , usrPassword = Just p
                                              }
      (usrUserID . snd $ createAccount now nilMessage nilU nilEmail (Just p))
        `shouldBe` nilU
      snd (createAccount now nilMessage nilU nilEmail (Just p))
        `shouldSatisfy` ((== nilU) . usrUserID)

  describe "updateAccount" $ do
    let now = stringToTime "2019-09-02T20:15:32+00:00"
    let model   = defaultUser now nilEmail
        myemail = "test@paytgthr.dev"

    it "accepts empty messages" $ do
      snd (updateAccount nilMessage [] ([], model))
        `shouldBe` model { usrUserState = USR.UserCreated }

    it "saves 1 key" $ do
      let changes = [(NameFirst, Just myemail)]
      let model'  = snd (updateAccount nilMessage changes ([], model))
      model' `shouldSatisfy` hasEmail myemail
      model' `shouldSatisfy` isCreated

  describe "approveRatio" $ do
    it "Sets both accepted and approved" $ do
      let
        user1 =
          UserID . fromJust . fromText $ "00000000-0000-0000-0000-000000000000"
        group = verifyGroupCohesion $ GroupModel
          { grpId        = GroupId nil
          , grpStatus    = GroupPending
          , grpStart     = Nothing
          , grpEnd       = Nothing
          , grpSplit     =
            [ GroupSplit { splApproved = False, splRatio = 50, splUser = user1 }
            , GroupSplit { splApproved = True
                         , splRatio    = 50
                         , splUser     = UserID nil
                         }
            ]
          , grpMembers = [ GroupMember { mbrAccepted = False, mbrUser = user1 }
                         , GroupMember { mbrAccepted = True
                                       , mbrUser     = UserID nil
                                       }
                         ]
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
        newGroup    = snd $ approveRatio user1 ([], group)
        expectedRes = group
          { grpSplit   =
            [ GroupSplit { splApproved = True, splRatio = 50, splUser = user1 }
            , GroupSplit { splApproved = True
                         , splRatio    = 50
                         , splUser     = UserID nil
                         }
            ]
          , grpMembers = [ GroupMember { mbrAccepted = True, mbrUser = user1 }
                         , GroupMember { mbrAccepted = True
                                       , mbrUser     = UserID nil
                                       }
                         ]
          }
      newGroup `shouldBe` expectedRes

    it "Sets only for user" $ do
      let
        user1 =
          UserID . fromJust . fromText $ "00000000-0000-0000-0000-000000000000"
        now   = unsafePerformIO getCurrentTime
        group = verifyGroupCohesion $ GroupModel
          { grpId        = GroupId nil
          , grpStatus    = GroupPending
          , grpStart     = Nothing
          , grpEnd       = Nothing
          , grpSplit     =
            [ GroupSplit { splApproved = False, splRatio = 50, splUser = user1 }
            , GroupSplit { splApproved = False
                         , splRatio    = 50
                         , splUser     = UserID nil
                         }
            ]
          , grpMembers = [ GroupMember { mbrAccepted = False, mbrUser = user1 }
                         , GroupMember { mbrAccepted = False
                                       , mbrUser     = UserID nil
                                       }
                         ]
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = now
          }
        newGroup    = snd $ approveRatio user1 ([], group)
        expectedRes = group
          { grpSplit   =
            [ GroupSplit { splApproved = True, splRatio = 50, splUser = user1 }
            , GroupSplit { splApproved = False
                         , splRatio    = 50
                         , splUser     = UserID nil
                         }
            ]
          , grpMembers = [ GroupMember { mbrAccepted = True, mbrUser = user1 }
                         , GroupMember { mbrAccepted = False
                                       , mbrUser     = UserID nil
                                       }
                         ]
          }
      newGroup `shouldBe` expectedRes

  describe "createGroup" $ do
    let groupDeepEval g = evaluate (grpSplit . snd $ g)  -- force lazy eval
    it "throws error for 0 people" $ do
      let mid      = MessageID nil
      let gid      = GroupId nil
      let inviter  = UserID nil
      let now      = unsafePerformIO getCurrentTime
      let executor = createGroup mid now gid inviter []

      groupDeepEval executor `shouldThrow` errorCall "Error: no members"

    it "throws error for 5 people" $ do
      let mid     = MessageID nil
      let gid     = GroupId nil
      let inviter = UserID nil
      let now     = unsafePerformIO getCurrentTime
      let executor = createGroup
            mid
            now
            gid
            inviter
            [UserID nil, UserID nil, UserID nil, UserID nil, UserID nil]

      groupDeepEval executor `shouldThrow` errorCall "Error: group too large"

    it "works for 1 person" $ do
      let adanId =
            UserID . fromJust $ fromText "00000000-0000-0000-0000-000000000000"
      let mid      = MessageID nil
      let gid      = GroupId nil
      let now      = unsafePerformIO getCurrentTime
      let executor = createGroup mid now gid adanId [adanId]

      executor
        `shouldBe` ( [EventGroupCreated gid adanId [adanId]]
                   , verifyGroupCohesion $ GroupModel
                     { grpId        = GroupId nil
                     , grpStatus    = GroupCreated
                     , grpStart     = Nothing
                     , grpEnd       = Nothing
                     , grpSplit     = [ GroupSplit { splApproved = False
                                                   , splRatio    = 100
                                                   , splUser     = adanId
                                                   }
                                      ]
                     , grpMembers   = [ GroupMember { mbrAccepted = True
                                                    , mbrUser     = adanId
                                                    }
                                      ]
                     , grpRevision  = 1
                     , grpVersion   = "1.0"
                     , grpMsgSource = MessageID nil
                     , grpCreatedAt = now
                     }
                   )

    it "works for 2 people" $ do
      let adanId =
            UserID . fromJust $ fromText "00000000-0000-0000-0000-000000000000"
      let bobId =
            UserID . fromJust $ fromText "00000000-0000-0000-0000-000000000000"
      let mid      = MessageID nil
      let gid      = GroupId nil
      let now      = unsafePerformIO getCurrentTime
      let executor = createGroup mid now gid adanId [adanId, bobId]

      executor
        `shouldBe` ( [EventGroupCreated gid adanId [adanId, bobId]]
                   , verifyGroupCohesion $ GroupModel
                     { grpId        = GroupId nil
                     , grpStatus    = GroupCreated
                     , grpStart     = Nothing
                     , grpEnd       = Nothing
                     , grpSplit     =
                       [ GroupSplit { splApproved = False
                                    , splRatio    = 50
                                    , splUser     = adanId
                                    }
                       , GroupSplit { splApproved = False
                                    , splRatio    = 50
                                    , splUser     = bobId
                                    }
                       ]
                     , grpMembers   = [ GroupMember { mbrAccepted = True
                                                    , mbrUser     = adanId
                                                    }
                                      , GroupMember { mbrAccepted = False
                                                    , mbrUser     = bobId
                                                    }
                                      ]
                     , grpRevision  = 1
                     , grpVersion   = "1.0"
                     , grpMsgSource = MessageID nil
                     , grpCreatedAt = now
                     }
                   )

    it "works for 3 people" $ do
      let adanId =
            UserID . fromJust $ fromText "00000000-0000-0000-0000-000000000000"
      let bobId =
            UserID . fromJust $ fromText "00000000-0000-0000-0000-000000000000"
      let cindyId =
            UserID . fromJust $ fromText "00000000-0000-0000-0000-000000000000"
      let mid      = MessageID nil
      let gid      = GroupId nil
      let now      = unsafePerformIO getCurrentTime
      let executor = createGroup mid now gid adanId [adanId, bobId, cindyId]

      executor
        `shouldBe` ( [EventGroupCreated gid adanId [adanId, bobId, cindyId]]
                   , verifyGroupCohesion $ GroupModel
                     { grpId        = GroupId nil
                     , grpStatus    = GroupCreated
                     , grpStart     = Nothing
                     , grpEnd       = Nothing
                     , grpSplit     =
                       [ GroupSplit { splApproved = False
                                    , splRatio    = 34
                                    , splUser     = adanId
                                    }
                       , GroupSplit { splApproved = False
                                    , splRatio    = 33
                                    , splUser     = bobId
                                    }
                       , GroupSplit { splApproved = False
                                    , splRatio    = 33
                                    , splUser     = cindyId
                                    }
                       ]
                     , grpMembers   =
                       [ GroupMember { mbrAccepted = True, mbrUser = adanId }
                       , GroupMember { mbrAccepted = False, mbrUser = bobId }
                       , GroupMember { mbrAccepted = False, mbrUser = cindyId }
                       ]
                     , grpRevision  = 1
                     , grpVersion   = "1.0"
                     , grpMsgSource = MessageID nil
                     , grpCreatedAt = now
                     }
                   )
