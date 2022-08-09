{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}


module Model.CardholderSpec
  ( spec
  ) where

import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy           ( readFile )
import           Prelude                 hiding ( filter
                                                , readFile
                                                )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse(..)
                                                , KYCEvent(..)
                                                )
import           Shared.Models.KYC              ( KYCFailureReasons(SSNScoreLow)
                                                , KycStatus(..)
                                                )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

spec :: Spec
spec = parallel $ do
  describe "AptoCardholderResponse" $ do
    it "Passed" $ do
      let
        msg = AptoCardholderResponse
          { accxId          = AptoCardholderId "crdhldr_a"
          , accxEmail       = "nikki+test@example.com"
          , accxKYCStatus   = Just Passed
          , accxNameFirst   = "Nic"
          , accxNameLast    = "Li"
          , acxcPhoneNumber = "+12341231234"
          , accxPayTgthrId  = "00000000-0000-0000-0000-000000000000"
          , accxCreatedAt   = stringToTime "2019-10-04T20:15:32+00:00"
          , accxCards       = []
          , accxEvent       = Just KYCStatusUpdate
          , accxLiveMode    = True
          , accxKYCPassedAt = Just
                                $ stringToTime "2019-10-04T20:15:38.719+00:00"
          }
      someJson <- readFile "test/json/apto-cardholder-passed.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "FilesRequired (IdentityFailure SSNMismatch)" $ do
      let msg = AptoCardholderResponse
            { accxId          = AptoCardholderId "crdhldr_a"
            , accxEmail       = "austin+test@example.com"
            , accxKYCStatus   = Just $ AutoVerifyFailed [SSNScoreLow]
            , accxNameFirst   = "Austin"
            , accxNameLast    = "Li"
            , acxcPhoneNumber = "+12341231234"
            , accxPayTgthrId  = "00000000-0000-0000-0000-000000000000"
            , accxCreatedAt   = stringToTime "2019-10-04T20:16:02+00:00"
            , accxCards       = []
            , accxEvent       = Just KYCStatusUpdate
            , accxLiveMode    = True
            , accxKYCPassedAt = Nothing
            }
      someJson <- readFile "test/json/apto-cardholder-upload-file.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "UnderReview" $ do
      let msg = AptoCardholderResponse
            { accxId          = AptoCardholderId "crdhldr_a"
            , accxEmail       = "austin+test@example.com"
            , accxKYCStatus   = Just $ AutoVerifyFailed [SSNScoreLow]
            , accxNameFirst   = "Austin"
            , accxNameLast    = "Li"
            , acxcPhoneNumber = "+12341231234"
            , accxPayTgthrId  = "00000000-0000-0000-0000-000000000000"
            , accxCreatedAt   = stringToTime "2019-10-04T20:16:02+00:00"
            , accxCards       = []
            , accxEvent       = Just KYCStatusUpdate
            , accxLiveMode    = True
            , accxKYCPassedAt = Nothing
            }
      someJson <- readFile "test/json/apto-cardholder-under-review.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "IdentityUpdate" $ do
      let
        msg = AptoCardholderResponse
          { accxId          = AptoCardholderId "crdhldr_a"
          , accxEmail       = "austin+test@example.com"
          , accxKYCStatus   = Just Passed
          , accxNameFirst   = "Austin"
          , accxNameLast    = "Smith"
          , acxcPhoneNumber = "+12341231234"
          , accxPayTgthrId  = "00000000-0000-0000-0000-000000000000"
          , accxCreatedAt   = stringToTime "2019-10-04T20:16:02+00:00"
          , accxCards       = []
          , accxEvent       = Just KYCIdentityUpdate
          , accxLiveMode    = True
          , accxKYCPassedAt = Just
                                $ stringToTime "2019-10-04T20:18:14.284+00:00"
          }
      someJson <- readFile "test/json/apto-cardholder-identity-update.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "IdentityUpdate - NotChecked" $ do
      let msg = AptoCardholderResponse
            { accxId          = AptoCardholderId "crdhldr_a"
            , accxEmail       = "noop@example.com"
            , accxKYCStatus   = Just $ AutoVerifyFailed []
            , accxNameFirst   = "David"
            , accxNameLast    = "Wells"
            , acxcPhoneNumber = "+12341231234"
            , accxPayTgthrId  = "00000000-0000-0000-0000-000000000000"
            , accxCreatedAt   = stringToTime "2020-06-02T21:44:56+00:00"
            , accxCards       = []
            , accxEvent       = Just KYCIdentityUpdate
            , accxLiveMode    = True
            , accxKYCPassedAt = Nothing
            }
      someJson <- readFile "test/json/apto-cardholder-identity-notchecked.json"
      A.eitherDecode someJson `shouldBe` Right msg

