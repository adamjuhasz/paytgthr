{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}


module App.SplitSpec where

import           Data.Maybe                     ( fromJust )
import           Data.Ratio                     ( (%) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( fromText )
import           PaymentAuth.App.Split          ( calculateSplit )
import           Shared.Models.Group            ( GroupSplit(..) )
import           Shared.Models.Transaction      ( FairShare )
import           Shared.Models.User             ( UserID(..) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

textToUID :: Text -> UserID
textToUID = UserID . fromJust . fromText

user1 :: UserID
user1 = textToUID "00000000-0000-0000-0000-000000000000"
user2 :: UserID
user2 = textToUID "00000000-0000-0000-0000-000000000000"
user3 :: UserID
user3 = textToUID "00000000-0000-0000-0000-000000000000"

addShares :: (UserID, FairShare) -> Rational -> Rational
addShares (_, share) accum = accum + share

addSplits :: GroupSplit -> Rational -> Rational
addSplits GroupSplit {..} accum = accum + splRatio

groupToSplit :: [GroupSplit] -> [(UserID, Rational)]
groupToSplit group = (\GroupSplit {..} -> (splUser, splRatio)) <$> group

toD :: Rational -> Double
toD = fromRational

spec :: Spec
spec = parallel $ do
  describe "calculateSplit" $ do
    it "Regression on -3.99" $ do
      let amount = (-2246170314151035) % 562949953421312
          group  = [GroupSplit user1 50 True, GroupSplit user2 50 True]
          shares = calculateSplit amount $ groupToSplit group
      fmap (toD . snd) shares `shouldBe` [-2, -1.99]
      toD (foldr addShares 0 shares) `shouldBe` toD amount
      foldr addSplits 0 group `shouldBe` 100

    it "divides even quant, even ratio" $ do
      let amount = 50.00
          group  = [GroupSplit user1 50 True, GroupSplit user2 50 True]
          shares = calculateSplit amount $ groupToSplit group
      shares `shouldBe` [(user1, 25), (user2, 25)]
      foldr addShares 0 shares `shouldBe` amount
      foldr addSplits 0 group `shouldBe` 100

    it "divides uneven quant, even ratio" $ do
      let amount = 50.01
          group  = [GroupSplit user1 50 True, GroupSplit user2 50 True]
          shares = calculateSplit amount $ groupToSplit group
      shares `shouldBe` [(user1, 25), (user2, 25.01)]
      foldr addShares 0 shares `shouldBe` amount
      foldr addSplits 0 group `shouldBe` 100

    it "divides even quant, diff ratio" $ do
      let amount = 100.00
          group  = [GroupSplit user1 30 True, GroupSplit user2 70 True]
          shares = calculateSplit amount $ groupToSplit group
      shares `shouldBe` [(user1, 30.00), (user2, 70.00)]
      foldr addShares 0 shares `shouldBe` amount
      foldr addSplits 0 group `shouldBe` 100

    it "divides uneven quant, diff ratio" $ do
      let amount = 79.33
          group  = [GroupSplit user1 30 True, GroupSplit user2 70 True]
          shares = calculateSplit amount $ groupToSplit group
      shares `shouldBe` [(user1, 23.80), (user2, 55.53)]
      foldr addShares 0 shares `shouldBe` amount
      foldr addSplits 0 group `shouldBe` 100

    describe "3 people" $ it "does 3 people, uneven" $ do
      let amount = 101.02
          group =
            [ GroupSplit user1 30 True
            , GroupSplit user2 30 True
            , GroupSplit user3 40 True
            ]
          shares = calculateSplit amount $ groupToSplit group
      shares `shouldBe` [(user1, 30.31), (user2, 30.30), (user3, 40.41)]
      foldr addShares 0 shares `shouldBe` amount
      foldr addSplits 0 group `shouldBe` 100


