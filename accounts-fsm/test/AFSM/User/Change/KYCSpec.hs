module AFSM.User.Change.KYCSpec where
{- HLINT ignore "Redundant do" -}

import           AFSM.Cognito.Client            ( CognitoProfileId(..)
                                                , IdentityAssessmentResponse
                                                , ScreeningStatus(..)
                                                )
import           AFSM.User.Change.KYC           ( processAssesment )
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.UUID                      ( nil )
import           Prelude                 hiding ( readFile )
import           Shared.Models.KYCAssesment     ( IdentitySearchId(..)
                                                , KYCAssesment(..)
                                                , KYCFailureReasons(..)
                                                )
import           Shared.Models.User             ( UserID(UserID) )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldSatisfy
                                                )

suckRightOut :: Either a b -> b
suckRightOut (Right x) = x
suckRightOut (Left  _) = error "is left"

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

      now <- getCurrentTime
      let assesment = suckRightOut decoded
      let assesed = processAssesment now
                                     (UserID nil)
                                     (CognitoProfileId "")
                                     (IdentitySearchId "")
                                     ScreeningCleared
                                     assesment

      scoreOverall assesed `shouldBe` 25
      failureReasons assesed
        `shouldBe` [NameScoreLow, DOBScoreLow, IdentityTheftRisk]
      return ()
