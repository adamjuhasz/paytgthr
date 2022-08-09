{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module APIPrivacy.Models.PrivacySpec
  ( spec
  ) where

import           APIPrivacy.Models.Privacy      ( PrivacyCard
                                                , Transaction
                                                )
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import           Prelude                 hiding ( readFile )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldSatisfy
                                                )

spec :: Spec
spec = parallel $ do
  describe "Card" $ do
    it "decodes" $ do
      someJson <- readFile "./test/APIPrivacy/Models/privacy-card.json"
      let decoded :: Either String PrivacyCard = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  e -> error $ show e
                        )
    it "decodes v2" $ do
      someJson <- readFile "./test/APIPrivacy/Models/privacy-card-v2.json"
      let decoded :: Either String PrivacyCard = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  e -> error $ show e
                        )

  describe "Transaction" $ do
    it "decodes" $ do
      someJson <- readFile "./test/APIPrivacy/Models/privacy-transaction.json"
      let decoded :: Either String Transaction = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  e -> error $ show e
                        )
