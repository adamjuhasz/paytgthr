{- HLINT ignore "Redundant do" -}

module Shared.Models.CurrencySpec where

import           Control.Exception              ( evaluate )
import           Data.Ratio                     ( (%) )
import           Shared.Models.Currency         ( Currency(Currency)
                                                , roundDown
                                                , roundUp
                                                , roundUpUSD
                                                )
import           Test.Hspec                     ( Spec
                                                , anyException
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldThrow
                                                )

spec :: Spec
spec = parallel $ do
  describe "(+)" $ do
    it "with fromInteger 0" $ do
      Currency "USD" 1 + 0 `shouldBe` Currency "USD" 1
      0 + Currency "USD" 1 `shouldBe` Currency "USD" 1

    it "with fromInteger" $ do
      Currency "USD" 1 + 1.2 `shouldBe` Currency "USD" 2.2
      1.2 + Currency "USD" 1 `shouldBe` Currency "USD" 2.2

    it "same ISO" $ do
      Currency "USD" 1 + Currency "USD" 1.1 `shouldBe` Currency "USD" 2.1

    it "different ISO" $ do
      evaluate (Currency "USD" 1 + Currency "EUR" 1.1)
        `shouldThrow` anyException

  describe "(-)" $ do
    it "with fromInteger" $ do
      Currency "USD" 1 - 0.5 `shouldBe` Currency "USD" 0.5
      0.5 - Currency "USD" 1 `shouldBe` Currency "USD" (-0.5)

    it "with fromInteger" $ do
      Currency "USD" 1 - 1.2 `shouldBe` Currency "USD" (-0.2)
      1.2 - Currency "USD" 1 `shouldBe` Currency "USD" 0.2

    it "same ISO" $ do
      Currency "USD" 1.2 - Currency "USD" 1.1 `shouldBe` Currency "USD" 0.1

    it "different ISO" $ do
      evaluate (Currency "USD" 1 - Currency "EUR" 1.1)
        `shouldThrow` anyException

  describe "(*)" $ do
    it "with fromInteger" $ do
      Currency "USD" 1 * 2 `shouldBe` Currency "USD" 2
      0.5 * Currency "USD" 1 `shouldBe` Currency "USD" 0.5

    it "same ISO" $ do
      Currency "USD" 1.2 * Currency "USD" 3 `shouldBe` Currency "USD" 3.6

    it "different ISO" $ do
      evaluate (Currency "USD" 1 * Currency "EUR" 1.1)
        `shouldThrow` anyException

  describe "(/)" $ do
    it "with fromInteger" $ do
      Currency "USD" 1 / 2 `shouldBe` Currency "USD" 0.5
      evaluate (2 / Currency "USD" 1) `shouldThrow` anyException

    it "same ISO" $ do
      Currency "USD" 4 / Currency "USD" 2 `shouldBe` Currency "USD" 2

    it "different ISO" $ do
      evaluate (Currency "USD" 1 / Currency "EUR" 1.1)
        `shouldThrow` anyException

  describe "Ord / Eq" $ do
    it "(>)" $ do
      Currency "USD" 2 > Currency "USD" 1 `shouldBe` True

    it "(==)" $ do
      Currency "USD" 2 == Currency "USD" 1 `shouldBe` False
      Currency "USD" 2 == Currency "USD" 2 `shouldBe` True

    it "fromInteger" $ do
      Currency "USD" 4 > 2 `shouldBe` True
      Currency "USD" 4 /= 0 `shouldBe` True
      Currency "USD" 0 /= 0 `shouldBe` False
      Currency "USD" 0 == 0 `shouldBe` True

    it "different ISO" $ do
      evaluate (Currency "USD" 1 > Currency "EUR" 1.1)
        `shouldThrow` anyException
      Currency "USD" 1 == Currency "EUR" 1.1 `shouldBe` False

  describe "roundUpUSD" $ do
    it "should not lose money" $ do
      let z   = Currency "USD" $ 0 % 100
      let p   = Currency "USD" $ 13183 % 100
      let res = roundUpUSD $ z + p
      res `shouldBe` p
      roundUpUSD z `shouldBe` z
      roundUpUSD p `shouldBe` p

  describe "roundUp" $ do
    it "shoudl work" $ do
      roundUp 2 (Currency "USD" $ 123121 % 1000)
        `shouldBe` Currency "USD" (12313 % 100)

  describe "roundDown" $ do
    it "shoudl work" $ do
      roundDown 2 (Currency "USD" $ 123121 % 1000)
        `shouldBe` Currency "USD" (12312 % 100)
