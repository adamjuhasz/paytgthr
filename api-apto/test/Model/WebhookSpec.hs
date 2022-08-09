{- HLINT ignore "Reduce duplication" -}

module Model.WebhookSpec where

import           APIApto.Model.Webhook          ( AptoTransaction(..)
                                                , AptoWebhook(..)
                                                , CardUpdate(..)
                                                , CardUpdateEvent(..)
                                                , SettlementId(..)
                                                , SettlementNotify(..)
                                                )
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import           Data.Ratio                     ( (%) )
import           Prelude                 hiding ( readFile )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Apto.Card        ( CardLastFour(..)
                                                , CardStatus(..)
                                                )
import           Shared.Models.Apto.Cardholder  ( AptoCardholderResponse(..)
                                                , KYCEvent(..)
                                                )
import           Shared.Models.Apto.Transaction ( AptoCardHolder(..)
                                                , AptoTransactionId(..)
                                                , MerchantInfo(..)
                                                , TransactionDetails(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.Card             ( AptoCardId(..)
                                                , CardDesign(..)
                                                )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , AptoAdjustmentID(..)
                                                , AptoAdjustmentType(..)
                                                , CardNetwork(..)
                                                , MastercardMCC(..)
                                                , PurchaseType(..)
                                                , TransactionEvent(..)
                                                )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldSatisfy
                                                )

spec :: Spec
spec = parallel $ do
  describe "Transaction Webhook" $ do
    it "does v1 gist" $ do
      someJson <- readFile "test/json/TransactionUpdatePending.json"
      let
        expected = TransactionNot $ AptoTransaction
          { aptSource           = Apto
          , aptIdempotency      = Nothing
          , aptSourceId         = AptoTransactionId "txn_a"
          , aptCardId           = AptoCardId "crd_c7015aecb8a05618"
          , aptDetails          = CardTransaction
                                    { pcpContext = "PIN purchase (VISA, Signature)"
                                    , pcpIsCardPresent   = True
                                    , pcpIsOnline        = False
                                    , pcpIsInternational = False
                                    , pcpNetwork         = Visa
                                    , pcpIsEMV           = False
                                    , pcpType            = Just Pin
                                    , pcpDescription     = Nothing
                                    }
          , aptCardholder       = AptoCardHolder
                                    { apcId        = AptoCardholderId
                                                       "crdhldr_a"
                                    , apcEmail     = "pat@example.com"
                                    , apcNameFirst = "Josh"
                                    , acpNameLast  = "Wilson"
                                    }
          , aptMerchant = CardMerchant { cmiMcc      = MastercardMCC "7299"
                                       , cmiMccDesc  = "Food and Beverage"
                                       , cmiName     = "LORDS PHARMACY LTD"
                                       , cmiLocality = Just "LONDON"
                                       , cmiRegion   = Nothing
                                       , cmiCountry  = "GBR"
                                       }
          , aptStandin          = False
          , aptState            = TrxPending
          , aptTransactionEvent = StateTransition
          , aptCreatedAt        = stringToTime "2016-10-19T23:25:17+00:00"
          , aptAmountLocal = Currency "USD"
                                      ((-3317182600535081) % 140737488355328)
          , aptAmountHold       = Currency "USD" 0
          , aptAmountCashback   = Currency "USD" 0
          , aptAmountFee        = Currency "USD" (5 % 2)
          , aptAmountBilling    = Currency "USD" 0
          , aptVersion          = "1.0"
          , aptAdjustments      =
            [ AptoAdjustment
                { adjId              = AptoAdjustmentID "adj_3bc9dcf557975fe4"
                , adjCreatedAt       = stringToTime "2018-07-04T15:15:51.00Z"
                , adjAmountLocal     = Currency
                                         "USD"
                                         ((-3317182600535081) % 140737488355328)
                , adjAmountBilling   = Currency
                                         "USD"
                                         ((-3317182600535081) % 140737488355328)
                , adjFSTransactionId = Just
                                         "00000000-0000-0000-0000-000000000000"
                , adjType            = CaptureAdjustment
                }
            ]
          }
      eitherDecode someJson `shouldBe` Right expected

    it "does v2" $ do
      someJson <- readFile "test/json/apto-webhook-v2-trx-update.json"
      eitherDecode someJson
        `shouldSatisfy` (\case
                          Right (TransactionNot _) -> True
                          _                        -> False
                        )


  describe "Cardholder Webhook" $ do
    it "null kyc" $ do
      -- gist JSON is not valid
      someJson <- readFile "test/json/apto-cardholder-identity-null-kyc.json"
      let msg = KYCNot $ AptoCardholderResponse
            { accxId          = AptoCardholderId "crdhldr_a"
            , accxEmail       = "cenzieann2015@example.com"
            , accxKYCStatus   = Nothing
            , accxNameFirst   = "Cayla"
            , accxNameLast    = "Perry"
            , acxcPhoneNumber = "+12341231234"
            , accxPayTgthrId  = "00000000-0000-0000-0000-000000000000"
            , accxCreatedAt   = stringToTime "2020-05-20T03:47:43+00:00"
            , accxCards       = []
            , accxEvent       = Just KYCIdentityUpdate
            , accxLiveMode    = True
            , accxKYCPassedAt = Nothing
            }
      eitherDecode someJson `shouldBe` Right msg

    it "does v2 kyc update" $ do
      someJson <- readFile
        "test/json/apto-webhook-v2-cardholder-kyc-update.json"
      let decoded = eitherDecode someJson

      decoded
        `shouldSatisfy` (\case
                          Right (KYCNot _) -> True
                          _                -> False
                        )
      decoded
        `shouldSatisfy` (\case
                          Right (KYCNot AptoCardholderResponse { accxEvent = Just _ })
                            -> True
                          _ -> False
                        )

    it "does v2 identity update" $ do
      someJson <- readFile
        "test/json/apto-webhook-v2-cardholder-iden-update.json"
      let decoded = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right (KYCNot _) -> True
                          _                -> False
                        )
      decoded
        `shouldSatisfy` (\case
                          Right (KYCNot AptoCardholderResponse { accxEvent = Just _ })
                            -> True
                          _ -> False
                        )

  describe "Card Webhook" $ do
    it "v1 gist" $ do
      someJson <- readFile "test/json/CardUpdate.json"
      let expected = CardNot $ CardUpdate
            { crdId           = AptoCardId "crd_fde2d61d233455b9"
            , crdCardholderId = AptoCardholderId "crdhldr_a"
            , crdDesign       = UnknownDesign "blue"
            , crdStatus       = CardActive
            , crdLastFour     = Just $ CardLastFour "9497"
            , crdCreated      = Just $ stringToTime "2016-09-27T00:54:09+00:00"
            , crdActivated    = Just $ stringToTime "2016-09-27T01:46:32+00:00"
            , crdProgram      = "Apto_GPR"
            , crdEvent        = PinUpdated
            }
      eitherDecode someJson `shouldBe` Right expected


    it "does v2 status update" $ do
      someJson <- readFile "test/json/apto-webhook-v2-card-status-update.json"
      let decoded = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right (CardNot _) -> True
                          _                 -> False
                        )
      decoded
        `shouldSatisfy` (\case
                          Right (CardNot CardUpdate { crdEvent = StatusUpdate })
                            -> True
                          _ -> False
                        )

    it "does v2 pin update" $ do
      someJson <- readFile "test/json/apto-webhook-v2-card-pin-update.json"
      let decoded = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right (CardNot _) -> True
                          _                 -> False
                        )
      decoded
        `shouldSatisfy` (\case
                          Right (CardNot CardUpdate { crdEvent = PinUpdated })
                            -> True
                          _ -> False
                        )

  describe "Settlement Webhook" $ it "gist" $ do
    someJson <- readFile "test/json/DailySettlement.json"
    let
      expected = SettlementNot $ SettlementNotify
        { setId            = SettlementId "stlmnt_5hfi35"
        , setDate          = stringToTime "2016-05-24T16:36:24+00:00"
        , setBalanceTotal  = 56943.05
        , setBalancePeriod = 6281.39
        , setCsvUrl        =
          "https://example.aptopayments.com/settlements/stlmnt_5hfi35"
        }
    eitherDecode someJson `shouldBe` Right expected

  describe "Rejects malformed" $ do
    it "rejects v2 with bad parent key" $ do
      someJson <- readFile "test/json/apto-webhook-v2-malformed-events.json"
      let decoded :: Either String AptoWebhook = eitherDecode someJson
      decoded
        `shouldBe` Left "Error in $: Key not recognized, I see: [\"events\"]"

    it "rejects v2 with bad data_type" $ do
      someJson <- readFile "test/json/apto-webhook-v2-malformed-datatype.json"
      let decoded :: Either String AptoWebhook = eitherDecode someJson
      decoded `shouldBe` Left "Error in $: data_type not recognized: \"other\""

    it "rejects v2 with missing keys" $ do
      someJson <- readFile
        "test/json/apto-webhook-v2-malformed-missing-key.json"
      let decoded :: Either String AptoWebhook = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Left  _ -> True
                          Right _ -> False
                        )

    it "rejects v2 with bad formatted value" $ do
      someJson <- readFile "test/json/apto-webhook-v2-malformed-bad-value.json"
      let decoded :: Either String AptoWebhook = eitherDecode someJson
      decoded
        `shouldBe` Left
                     "Error in $.event['created_at']: could not parse date: Failed reading: date must be of form [+,-]YYYY-MM-DD"
