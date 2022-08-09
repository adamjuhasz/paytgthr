module AFSM.Web.Referral.ReferralSpec where

import           AFSM.Web.Referral.Referral
import           Data.Time.Clock
import           Data.UUID.V4
import           GHC.IO.Unsafe
import           Shared.Models.Ids
import           Shared.Models.Referral.ReferralProgress
import           Test.Hspec

exampleProg :: ReferralProgress
exampleProg = ReferralProgress
  { progressId        = ReferralProgressID $ unsafePerformIO nextRandom
  , referalProgram    = ReferralProgramID $ unsafePerformIO nextRandom
  , referee           = UserID $ unsafePerformIO nextRandom
  , referrer          = Just $ UserID $ unsafePerformIO nextRandom
  , programExpiration = Nothing
  , progress          = ProgramCompleted
  , progressDisplay   = 100
  , progressCreatedAt = unsafePerformIO getCurrentTime
  , progressUpdatedAt = unsafePerformIO getCurrentTime
  , progressRevision  = 1
  }

spec :: Spec
spec = do
  describe "referrReward" $ do
    it "calcs with none" $ do
      referrReward [] [(0, 1 :: Int), (5, 2 :: Int)] `shouldBe` Just 1

    it "calcs with 1" $ do
      referrReward [exampleProg] [(0, 1 :: Int), (5, 2 :: Int)]
        `shouldBe` Just 1

    it "calcs with 5" $ do
      referrReward
          [exampleProg, exampleProg, exampleProg, exampleProg, exampleProg]
          [(0, 1 :: Int), (5, 2 :: Int)]
        `shouldBe` Just 2

    it "calcs with 6" $ do
      referrReward
          [exampleProg, exampleProg, exampleProg, exampleProg, exampleProg]
          [(0, 1 :: Int), (5, 2 :: Int)]
        `shouldBe` Just 2

    it "calcs with 7" $ do
      referrReward
          [ exampleProg
          , exampleProg
          , exampleProg
          , exampleProg
          , exampleProg
          , exampleProg
          , exampleProg
          ]
          [(0, 1 :: Int), (5, 2 :: Int), (7, 3 :: Int)]
        `shouldBe` Just 3

    it "calcs with pending & expired" $ do
      referrReward
          [ exampleProg { progress = PurchaseCountProgress 1 3 }
          , exampleProg { progress = ProgramExpired }
          , exampleProg { progress = ProgramExpired }
          , exampleProg
          , exampleProg
          , exampleProg
          , exampleProg
          ]
          [(0, 1 :: Int), (5, 2 :: Int), (7, 3 :: Int)]
        `shouldBe` Just 1
