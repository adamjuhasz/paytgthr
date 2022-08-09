{- HLINT ignore "Redundant do" -}

module Handlers.Transactions.SafeSpendSpec where

import           Data.Maybe                     ( fromJust )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( fromText
                                                , nil
                                                )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           LandingPage.Handlers.Transactions.Utils
                                                ( calculateMinimum )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Group            ( GroupId(GroupId)
                                                , GroupModel(..)
                                                , GroupSplit
                                                  ( GroupSplit
                                                  , splApproved
                                                  , splRatio
                                                  , splUser
                                                  )
                                                , GroupStatus(GroupActive)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

uidMaker :: Text -> UserID
uidMaker = UserID . fromJust . fromText

spec :: Spec
spec = do
  describe "calculateMinimum" $ do
    let
      user1     = uidMaker "00000000-0000-0000-0000-000000000000"
      user2     = uidMaker "00000000-0000-0000-0000-000000000000"
      baseGroup = GroupModel
        { grpId        = GroupId nil
        , grpStatus    = GroupActive
        , grpStart     = Nothing
        , grpEnd       = Nothing
        , grpSplit     = [ GroupSplit { splUser     = user1
                                      , splRatio    = 60
                                      , splApproved = True
                                      }
                         , GroupSplit { splUser     = user2
                                      , splRatio    = 40
                                      , splApproved = True
                                      }
                         ]
        , grpMembers   = []
        , grpRevision  = 1
        , grpVersion   = "1.0"
        , grpMsgSource = MessageID nil
        , grpCreatedAt = unsafePerformIO getCurrentTime
        }
      getUersGroup   = baseGroup
      getUersGroup50 = baseGroup
        { grpSplit = [ GroupSplit { splUser     = user1
                                  , splRatio    = 50
                                  , splApproved = True
                                  }
                     , GroupSplit { splUser     = user2
                                  , splRatio    = 50
                                  , splApproved = True
                                  }
                     ]
        }
      getUsersLimitZero _ = return $ Left ("Unknown user" :: String)
      getUsersLimit uid | uid == user1 = return . Right $ Currency "USD" 100
                        | uid == user2 = return . Right $ Currency "USD" 50
                        | otherwise = return $ Left ("Unknown user" :: String)
      getUsersLimitNeg uid
        | uid == user1 = return . Right $ Currency "USD" 100
        | uid == user2 = return . Right $ Currency "USD" (-50)
        | otherwise    = return $ Left ("Unknown user" :: String)
      usersLevel1 uid | uid == user1 = return . Right $ Currency "USD" 20
                      | uid == user2 = return . Right $ Currency "USD" 20
                      | otherwise    = return $ Left ("Unknown user" :: String)
    it "calculates correctly for missing users" $ do
      val <- calculateMinimum getUersGroup getUsersLimitZero
      val `shouldBe` Currency "USD" 0

    it "calculates correctly for users" $ do
      let exected = 50.0 / 0.4
      val <- calculateMinimum getUersGroup getUsersLimit
      val `shouldBe` Currency "USD" exected

    it "calculates correctly for users" $ do
      val <- calculateMinimum getUersGroup getUsersLimitNeg
      val `shouldBe` Currency "USD" 0

    it "passes simple 50/50 level 1" $ do
      val <- calculateMinimum getUersGroup50 usersLevel1
      val `shouldBe` Currency "USD" 40
