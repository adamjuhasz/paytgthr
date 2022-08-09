{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Web.User.Change.IdentifySpec where

import           AFSM.Web.User.Change.Identify  ( isBannedCarrier
                                                , isBannedIpCountry
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

spec :: Spec
spec = parallel $ do
  describe "isBannedCarrier" $ do
    it "bans fuzzy" $ do
      isBannedCarrier "Vodafone HU" `shouldBe` True
      isBannedCarrier "JAZZ" `shouldBe` True

    it "happy path" $ do
      isBannedCarrier "Verizon" `shouldBe` False

  describe "isBannedIpCountry" $ do
    it "allows USA" $ do
      isBannedIpCountry "US" `shouldBe` False

    it "bans Russia" $ do
      isBannedIpCountry "RU" `shouldBe` True

