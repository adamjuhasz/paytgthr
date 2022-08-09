{- HLINT ignore "Redundant do" -}
module APIDwolla.Dwolla.TypesSpec where

import           Prelude                 hiding ( readFile )
import           APIDwolla.Dwolla.Types         ( DwollaError(..)
                                                , DwollaLink(..)
                                                , DwollaErrorReason(..)
                                                , DwollaEmbeddedError(..)
                                                , DwollaValidationErrorCode(..)
                                                )
import           Data.Aeson                    as A
                                                ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import           Test.Hspec                     ( describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , Spec
                                                )

spec :: Spec
spec = parallel $ do
  describe "DwollaError" $ do
    it "DuplicateResource" $ do
      let
        msg = DwollaError
          { errorCode    =
            DwollaDuplicateResource
              (DwollaLink
                { linkHref         =
                  "https://api.dwolla.com/funding-sources/00000000-0000-0000-0000-000000000000"
                , linkType         = "application/vnd.dwolla.v1.hal+json"
                , linkResourceType = "funding-source"
                }
              )
          , errorMessage =
            "Bank already exists: id=00000000-0000-0000-0000-000000000000"
          }
      someJson <- readFile "test/json/DuplicateResource.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "ValidationError - Restricted" $ do
      let
        msg = DwollaError
          { errorCode    = DwollaValidationError
                             [ DwollaEmbeddedError
                                 { validationError = Restricted
                                 , validationMessage = "Receiver restricted."
                                 , validationPath = "/_links/destination/href"
                                 }
                             ]
          , errorMessage =
            "Validation error(s) present. See embedded errors list for more details."
          }
      someJson <- readFile "test/json/Restricted.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "ValidationError - Invalid" $ do
      let
        msg = DwollaError
          { errorCode    = DwollaValidationError
                             [ DwollaEmbeddedError
                                 { validationError   = InvalidFundingSource
                                 , validationMessage = "Invalid funding source."
                                 , validationPath    = "/_links/source/href"
                                 }
                             ]
          , errorMessage =
            "Validation error(s) present. See embedded errors list for more details."
          }
      someJson <- readFile "test/json/Invalid.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "ValidationError - InvalidFormat" $ do
      let
        msg = DwollaError
          { errorCode    =
            DwollaValidationError
              [ DwollaEmbeddedError
                  { validationError   = InvalidFormat
                  , validationMessage =
                    "Invalid amount. The supplied amount must be a positive number."
                  , validationPath    = "/amount/value"
                  }
              ]
          , errorMessage =
            "Validation error(s) present. See embedded errors list for more details."
          }
      someJson <- readFile "test/json/InvalidFormat.json"
      A.eitherDecode someJson `shouldBe` Right msg
