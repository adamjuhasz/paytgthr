{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Web.SyncSegmentSpec where

import           AFSM.Web.Admin.SyncSegment     ( topGroupState )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( nil )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupModel(..)
                                                , GroupStatus(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

spec :: Spec
spec = parallel $ do
  describe "topGroupState" $ do
    it "sorts correctly" $ do
      let primaryGroup = GroupModel
            { grpId        = GroupId nil
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
      topGroupState [] `shouldBe` Nothing
      topGroupState
          [ primaryGroup { grpStatus = GroupClosed }
          , primaryGroup { grpStatus = GroupActive }
          ]
        `shouldBe` Just (primaryGroup { grpStatus = GroupActive })
      topGroupState
          [ primaryGroup { grpStatus = GroupClosed }
          , primaryGroup { grpStatus = GroupActive }
          , primaryGroup { grpStatus = GroupPending }
          , primaryGroup { grpStatus = GroupPaused }
          , primaryGroup { grpStatus = GroupCreated }
          , primaryGroup { grpStatus = GroupClosed }
          , primaryGroup { grpStatus = GroupExpired }
          , primaryGroup { grpStatus = GroupDenied }
          ]
        `shouldBe` Just (primaryGroup { grpStatus = GroupActive })
      topGroupState
          [ primaryGroup { grpStatus = GroupClosed }
          , primaryGroup { grpStatus = GroupPending }
          , primaryGroup { grpStatus = GroupPaused }
          , primaryGroup { grpStatus = GroupCreated }
          , primaryGroup { grpStatus = GroupClosed }
          , primaryGroup { grpStatus = GroupExpired }
          , primaryGroup { grpStatus = GroupDenied }
          ]
        `shouldBe` Just (primaryGroup { grpStatus = GroupPending })
