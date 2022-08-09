{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module Model.AptoAPIErrorSpec where

import           Prelude                 hiding ( readFile )
import           Test.Hspec
import           Data.Aeson
import           Data.ByteString.Lazy           ( readFile )

import           APIApto.Model.AptoAPIError

spec :: Spec
spec = parallel $ do
  describe "JSON decode" $ do
    it "pinoffset" $ do
      someJson <- readFile "test/json/apto-error-pinoffset.json"
      let expected = InvalidRequestError OfflinePINOffset
      eitherDecode someJson `shouldBe` Right expected

    it "temporarytrouble" $ do
      someJson <- readFile "test/json/apto-error-temporarytrouble.json"
      let expected = TemporaryAPIError
      eitherDecode someJson `shouldBe` Right expected

    it "phonenumber" $ do
      someJson <- readFile "test/json/apto-error-phonenumber.json"
      let expected = InvalidRequestError
            $ UnknownInvalid "User phone number +12341231234 is not valid"
      eitherDecode someJson `shouldBe` Right expected
