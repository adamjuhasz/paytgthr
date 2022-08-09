{- HLINT ignore "Redundant do" -}

module Shared.Models.RiskScoreSpec where

import           Data.UUID
import           Shared.Models.RiskScore
import           Shared.Models.User
import           Shared.TgthrMessages.Base
import           Shared.Utils
import           Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "progressToNextLevel" $ do
    let r = RiskScore { rskUser       = UserID nil
                      , rskRev        = 1
                      , rskTrustScore = 11
                      , rskChange     = 0
                      , rskFact       = ManualRiskAdj 11
                      , rskMsgSource  = MessageID nil
                      , rskCreatedAt  = stringToTime "2019-10-04T20:16:02+00:00"
                      }

    it "works for level 1 to 2" $ do
      progressToNextLevel (r { rskTrustScore = 11 }) `shouldBe` 0.07
      progressToNextLevel (r { rskTrustScore = 19 }) `shouldBe` 0.6

    it "works for level 10" $ do
      progressToNextLevel (r { rskTrustScore = 100 }) `shouldBe` 1.0

    it "works for level 0" $ do
      progressToNextLevel (r { rskTrustScore = 5 }) `shouldBe` 0.50
