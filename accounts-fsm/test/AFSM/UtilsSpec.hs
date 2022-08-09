{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.UtilsSpec where

import           AFSM.Utils                     ( hasUserAccepted
                                                , hasUserApproved
                                                , sortGroups
                                                )
import           Control.Monad                  ( when )
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromJust )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupMember
                                                  ( GroupMember
                                                  , mbrAccepted
                                                  , mbrUser
                                                  )
                                                , GroupModel(..)
                                                , GroupSplit
                                                  ( GroupSplit
                                                  , splApproved
                                                  , splRatio
                                                  , splUser
                                                  )
                                                , GroupStatus
                                                  ( GroupActive
                                                  , GroupClosed
                                                  , GroupPending
                                                  )
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                , shouldNotBe
                                                )


oneUser :: UserID
oneUser = UserID . fromJust . fromText $ "00000000-0000-0000-0000-000000000000"

twoUser :: UserID
twoUser = UserID . fromJust . fromText $ "00000000-0000-0000-0000-000000000000"

threeUser :: UserID
threeUser =
  UserID . fromJust . fromText $ "00000000-0000-0000-0000-000000000000"

spec :: Spec
spec = do
  describe "hasUserApproved" $ do
    let splits =
          [ GroupSplit { splUser = oneUser, splRatio = 0.5, splApproved = True }
          , GroupSplit { splUser     = twoUser
                       , splRatio    = 0.5
                       , splApproved = False
                       }
          ]
    it "finds user that did approve" $ do
      hasUserApproved oneUser splits `shouldBe` True
      hasUserApproved oneUser (reverse splits) `shouldBe` True

    it "finds user that did not approve" $ do
      hasUserApproved twoUser splits `shouldBe` False
      hasUserApproved twoUser (reverse splits) `shouldBe` False

    it "returns false for missing user" $ do
      hasUserApproved threeUser splits `shouldBe` False
      hasUserApproved threeUser (reverse splits) `shouldBe` False

  describe "hasUserAccepted" $ do
    let members =
          [ GroupMember { mbrUser = oneUser, mbrAccepted = True }
          , GroupMember { mbrUser = twoUser, mbrAccepted = False }
          ]
    it "finds user that did accepted invite" $ do
      hasUserAccepted oneUser members `shouldBe` True
      hasUserAccepted oneUser (reverse members) `shouldBe` True


    it "finds user that did not accepted invite" $ do
      hasUserAccepted twoUser members `shouldBe` False
      hasUserAccepted twoUser (reverse members) `shouldBe` False

    it "returns false for missing user" $ do
      hasUserAccepted threeUser members `shouldBe` False
      hasUserAccepted threeUser (reverse members) `shouldBe` False

  describe "sortGroups" $ do
    let primaryGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupActive
          , grpStart     = Nothing
          , grpEnd       = Nothing
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let octoberGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupActive
          , grpStart     = Just $ stringToTime "2019-10-01T20:15:32+00:00"
          , grpEnd       = Just $ stringToTime "2019-11-01T20:15:32+00:00"
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let septemberGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupActive
          , grpStart     = Just $ stringToTime "2019-09-02T20:15:32+00:00"
          , grpEnd       = Just $ stringToTime "2019-09-12T20:15:32+00:00"
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let pendingSeptemberGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupPending
          , grpStart     = Just $ stringToTime "2019-09-02T20:15:32+00:00"
          , grpEnd       = Just $ stringToTime "2019-09-12T20:15:32+00:00"
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let midOctoberGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupActive
          , grpStart     = Just $ stringToTime "2019-10-02T20:15:32+00:00"
          , grpEnd       = Just $ stringToTime "2019-10-12T20:15:32+00:00"
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let novemberGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupActive
          , grpStart     = Just $ stringToTime "2019-11-01T20:15:32+00:00"
          , grpEnd       = Just $ stringToTime "2019-12-01T20:15:32+00:00"
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let pendingGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupPending
          , grpStart     = Just $ stringToTime "2019-10-01T20:15:32+00:00"
          , grpEnd       = Just $ stringToTime "2019-11-01T20:15:32+00:00"
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let closedGroup = GroupModel
          { grpId        = (GroupId . fromJust . fromText)
                             "00000000-0000-0000-0000-000000000000"
          , grpStatus    = GroupClosed
          , grpStart     = Nothing
          , grpEnd       = Nothing
          , grpSplit     = []
          , grpMembers   = []
          , grpRevision  = 1
          , grpVersion   = "1.0"
          , grpMsgSource = MessageID nil
          , grpCreatedAt = unsafePerformIO getCurrentTime
          }
    let now = stringToTime "2019-10-04T20:15:32+00:00"

    it "sorts temp and primary with expired/future dates" $ do
      let groupIn =
            [ primaryGroup
            , closedGroup
            , pendingGroup
            , septemberGroup
            , midOctoberGroup
            , octoberGroup
            , novemberGroup
            , pendingSeptemberGroup
            ]

      when (length (nub groupIn) /= length groupIn)
           (error "groups are not uniq")

      sortGroups now groupIn
        `shouldBe` [ midOctoberGroup       -- temp active ends sooners
                   , octoberGroup          -- temp active ends after
                   , primaryGroup          -- primary
                   , novemberGroup         -- active but upcoming
                   , pendingGroup          -- pending
                   , septemberGroup        -- expired
                   , pendingSeptemberGroup -- expired
                   , closedGroup           -- closed
                   ]

    it "has a stable sort" $ do
      sortGroups now [midOctoberGroup, octoberGroup]
        `shouldBe` [midOctoberGroup, octoberGroup]
      sortGroups now [octoberGroup, midOctoberGroup]
        `shouldBe` [midOctoberGroup, octoberGroup]
      sortGroups now [octoberGroup, midOctoberGroup]
        `shouldNotBe` [octoberGroup, midOctoberGroup]

      sortGroups now [primaryGroup, midOctoberGroup]
        `shouldBe` [midOctoberGroup, primaryGroup]
      sortGroups now [primaryGroup, midOctoberGroup]
        `shouldNotBe` [primaryGroup, midOctoberGroup]

      sortGroups now [midOctoberGroup, primaryGroup]
        `shouldBe` [midOctoberGroup, primaryGroup]
      sortGroups now [midOctoberGroup, primaryGroup]
        `shouldNotBe` [primaryGroup, midOctoberGroup]

      sortGroups now [primaryGroup, primaryGroup]
        `shouldBe` [primaryGroup, primaryGroup]
