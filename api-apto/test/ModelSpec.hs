{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}
module ModelSpec where

import           APIApto.Model                  ( IdempotencyKey(..)
                                                , TranactionAuthRequest(..)
                                                , TrxDirection(..)
                                                )
import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy           ( readFile )
import           Prelude                 hiding ( readFile )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Apto.Transaction ( AptoTransactionId(..)
                                                , MerchantInfo(..)
                                                , TransactionDetails(..)
                                                )
import           Shared.Models.Card             ( AptoCardId(AptoCardId) )
import           Shared.Models.Transaction      ( CardNetwork(..)
                                                , MastercardMCC(..)
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldSatisfy
                                                )

spec :: Spec
spec = parallel $ do
  describe "TranactionAuthRequest" $ do
    it "accepts V1 of the auth body" $ do
      let msg = TranactionAuthRequest
            { tarType           = DebitTransaction
            , tarAmount         = "6.0"
            , tarCurrency       = "USD"
            , tarTransactionId  = AptoTransactionId "txn_a"
            , tarIdempotencyKey = IdempotencyKey "hold-5013518"
            , tarDescription    = "CHEVRON 0091623"
            , tarMerchant       = Nothing
            , tarDetails        = Nothing
            , tarCardholderId   = Nothing
            , tarCardId         = Nothing
            }
      someJson <- readFile "test/json/apto-auth-webhook-v1.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "accepts V2 of the auth body" $ do
      let
        msg = TranactionAuthRequest
          { tarType           = DebitTransaction
          , tarAmount         = "6.0"
          , tarCurrency       = "USD"
          , tarTransactionId  = AptoTransactionId "txn_a"
          , tarIdempotencyKey = IdempotencyKey "hold-5013518"
          , tarDescription    = "CHEVRON 0091623"
          , tarMerchant = Just $ CardMerchant { cmiName     = "CHEVRON 0091623"
                                              , cmiLocality = Just "Irvine"
                                              , cmiRegion   = Just "CA"
                                              , cmiCountry  = "USA"
                                              , cmiMcc = MastercardMCC "4829"
                                              , cmiMccDesc  = "Money Orders"
                                              }
          , tarDetails        = Just $ CardTransaction
            { pcpIsCardPresent   = True
            , pcpIsOnline        = False
            , pcpIsInternational = False
            , pcpIsEMV           = True
            , pcpNetwork         = Mastercard
            , pcpType            = Nothing
            , pcpContext         = "Authorization Request (PayTgthr internal)"
            , pcpDescription     = Nothing
            }
          , tarCardholderId   = Nothing
          , tarCardId         = Nothing
          }
      someJson <- readFile "test/json/apto-auth-webhook-v2.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "accepts V3 of the auth body" $ do
      let
        msg = TranactionAuthRequest
          { tarType           = DebitTransaction
          , tarAmount         = "23.57"
          , tarCurrency       = "USD"
          , tarTransactionId  = AptoTransactionId "txn_a"
          , tarIdempotencyKey = IdempotencyKey "auth_a"
          , tarDescription    = "LORDS PHARMACY LTD"
          , tarMerchant = Just $ CardMerchant { cmiName = "LORDS PHARMACY LTD"
                                              , cmiLocality = Just "LONDON"
                                              , cmiRegion = Nothing
                                              , cmiCountry = "GBR"
                                              , cmiMcc = MastercardMCC "7299"
                                              , cmiMccDesc = "Unknown"
                                              }
          , tarDetails        = Just $ CardTransaction
            { pcpIsCardPresent   = True
            , pcpIsOnline        = False
            , pcpIsInternational = True
            , pcpIsEMV           = True
            , pcpNetwork         = Mastercard
            , pcpType            = Nothing
            , pcpContext         = "Authorization Request (PayTgthr internal)"
            , pcpDescription     = Nothing
            }
          , tarCardholderId = Just $ AptoCardholderId "crdhldr_a"
          , tarCardId         = Just $ AptoCardId "card_a"
          }
      someJson <- readFile "test/json/apto-auth-webhook-v3-amount-number.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "accepts V3 of the auth body with string amounts" $ do
      let
        msg = TranactionAuthRequest
          { tarType           = DebitTransaction
          , tarAmount         = "23.57"
          , tarCurrency       = "USD"
          , tarTransactionId  = AptoTransactionId "txn_a"
          , tarIdempotencyKey = IdempotencyKey "auth_a"
          , tarDescription    = "LORDS PHARMACY LTD"
          , tarMerchant = Just $ CardMerchant { cmiName = "LORDS PHARMACY LTD"
                                              , cmiLocality = Just "LONDON"
                                              , cmiRegion = Nothing
                                              , cmiCountry = "GBR"
                                              , cmiMcc = MastercardMCC "7299"
                                              , cmiMccDesc = "Unknown"
                                              }
          , tarDetails        = Just $ CardTransaction
            { pcpIsCardPresent   = True
            , pcpIsOnline        = False
            , pcpIsInternational = True
            , pcpIsEMV           = True
            , pcpNetwork         = Mastercard
            , pcpType            = Nothing
            , pcpContext         = "Authorization Request (PayTgthr internal)"
            , pcpDescription     = Nothing
            }
          , tarCardholderId = Just $ AptoCardholderId "crdhldr_a"
          , tarCardId         = Just $ AptoCardId "card_a"
          }
      someJson <- readFile "test/json/apto-auth-webhook-v3-amount-string.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "accepts V3 of the auth body with null fields" $ do
      let
        msg = TranactionAuthRequest
          { tarType           = DebitTransaction
          , tarAmount         = "23.57"
          , tarCurrency       = "USD"
          , tarTransactionId  = AptoTransactionId "txn_a"
          , tarIdempotencyKey = IdempotencyKey "auth_a"
          , tarDescription    = "Merchant"
          , tarMerchant       = Nothing
          , tarDetails        = Just $ CardTransaction
            { pcpIsCardPresent   = True
            , pcpIsOnline        = False
            , pcpIsInternational = True
            , pcpIsEMV           = True
            , pcpNetwork         = UnknownNetwork "null"
            , pcpType            = Nothing
            , pcpContext         = "Authorization Request (PayTgthr internal)"
            , pcpDescription     = Nothing
            }
          , tarCardholderId = Just $ AptoCardholderId "crdhldr_a"
          , tarCardId         = Just $ AptoCardId "card_a"
          }
      someJson <- readFile "test/json/apto-auth-webhook-v3-null-fields.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "accepts V3 of the auth body from online doc example" $ do
      -- https://docs.aptopayments.com/api/core-api/#operation/authorization_approve
      -- accessed: 2021-04-12
      someJson <- readFile
        "test/json/apto-auth-webhook-v3-online-doc-example.json"
      A.eitherDecode someJson
        `shouldSatisfy` (\case
                          Right TranactionAuthRequest{} -> True
                          Left  _                       -> False
                        )


    it "rejects invalid" $ do
      someJson <- readFile "test/json/apto-auth-webhook-invalid.json"
      let decoded :: Either String TranactionAuthRequest =
            A.eitherDecode someJson
      decoded `shouldBe` Left "Error in $: empty"
