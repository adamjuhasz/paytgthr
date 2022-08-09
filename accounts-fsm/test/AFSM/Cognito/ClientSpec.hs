module AFSM.Cognito.ClientSpec where
{- HLINT ignore "Redundant do" -}

import           AFSM.Cognito.Client
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
  describe "Assesment" $ do
    it "decodes" $ do
      someJson <- readFile "./test/AFSM/Cognito/assesment.json"
      let decoded :: Either String IdentityAssessmentResponse =
            eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  e -> error $ show e
                        )

  describe "BirthComparison" $ do
    it "decodes" $ do
      someJson <- readFile "./test/AFSM/Cognito/birth_comparison.json"
      let decoded :: Either String BirthComparison = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  e -> error $ show e
                        )

  describe "SSNComparison" $ do
    it "decodes" $ do
      someJson <- readFile "./test/AFSM/Cognito/ssn_comparison.json"
      let decoded :: Either String SSNComparison = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  e -> error $ show e
                        )


