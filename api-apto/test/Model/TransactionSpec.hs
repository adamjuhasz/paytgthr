{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module Model.TransactionSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile
                                                , filter
                                                )
import           Test.Hspec
import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy           ( readFile )
import           Data.Ratio

import           APIApto.Model.Webhook
import           Shared.Models.Apto.Base
import           Shared.Models.Apto.Transaction
import           Shared.Utils
import           Shared.Models.Card
import           Shared.Models.Currency
import           Shared.Models.Transaction

spec :: Spec
spec = parallel $ do
  describe "Transaction" $ do
    it "Declined" $ do
      let
        msg = AptoTransaction
          { aptSource           = Apto
          , aptIdempotency      = Nothing
          , aptSourceId         = AptoTransactionId "txn_a"
          , aptCardId           = AptoCardId "crd_504417abf2582de3"
          , aptDetails          =
            CardTransaction
              { pcpContext         = "Decline. Insufficient funds. (MASTERCARD)"
              , pcpIsCardPresent   = False
              , pcpIsOnline        = True
              , pcpIsInternational = False
              , pcpNetwork         = Mastercard
              , pcpIsEMV           = False
              , pcpType            = Nothing
              , pcpDescription = Just "APL* ITUNES.COM/BILL 866-712-7753 CAUSA"
              }
          , aptCardholder       = AptoCardHolder
                                    { apcId        = AptoCardholderId
                                                       "crdhldr_a"
                                    , apcEmail = "nikki+test@example.com"
                                    , apcNameFirst = "Nicole"
                                    , acpNameLast  = "Wong"
                                    }
          , aptMerchant = CardMerchant { cmiMcc      = MastercardMCC "5735"
                                       , cmiMccDesc  = "Record Stores"
                                       , cmiName     = "APL* ITUNES.COM/BILL"
                                       , cmiLocality = Just "866-712-7753"
                                       , cmiRegion   = Just "CA"
                                       , cmiCountry  = "USA"
                                       }
          , aptStandin          = False
          , aptState            = TrxDeclined (LowBalance [])
          , aptCreatedAt        = stringToTime "2019-10-04T21:35:11+00:00"
          , aptTransactionEvent = StateTransition
          , aptAmountLocal      = Currency "USD" (-2)
          , aptAmountHold       = Currency "USD" 0
          , aptAmountCashback   = Currency "USD" 0
          , aptAmountFee        = Currency "USD" 0
          , aptAmountBilling    = Currency "USD" 0
          , aptVersion          = "1.0"
          , aptAdjustments      = []
          }
      someJson <- readFile "test/json/apto-transaction-declined.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Balance Inqiry" $ do
      let
        msg = AptoTransaction
          { aptSource           = Apto
          , aptIdempotency      = Nothing
          , aptSourceId         = AptoTransactionId "txn_a"
          , aptCardId           = AptoCardId "crd_504417abf2582de3"
          , aptDetails          =
            CardTransaction
              { pcpContext         = "Balance inquiry (MASTERCARD)"
              , pcpIsCardPresent   = False
              , pcpIsOnline        = True
              , pcpIsInternational = False
              , pcpNetwork         = Mastercard
              , pcpIsEMV           = False
              , pcpType            = Nothing
              , pcpDescription = Just "APL* ITUNES.COM/BILL 866-712-7753 CAUSA"
              }
          , aptCardholder       = AptoCardHolder
                                    { apcId        = AptoCardholderId
                                                       "crdhldr_a"
                                    , apcEmail = "nikki+test@example.com"
                                    , apcNameFirst = "Nicole"
                                    , acpNameLast  = "Wong"
                                    }
          , aptMerchant = CardMerchant { cmiMcc      = MastercardMCC "5735"
                                       , cmiMccDesc  = "Record Stores"
                                       , cmiName     = "APL* ITUNES.COM/BILL"
                                       , cmiLocality = Just "866-712-7753"
                                       , cmiRegion   = Just "CA"
                                       , cmiCountry  = "USA"
                                       }
          , aptStandin          = False
          , aptState            = TrxCompleted
          , aptCreatedAt        = stringToTime "2019-10-04T21:34:49+00:00"
          , aptTransactionEvent = BalanceInquiry
          , aptAmountLocal      = Currency "USD" 0
          , aptAmountHold       = Currency "USD" 0
          , aptAmountCashback   = Currency "USD" 0
          , aptAmountFee        = Currency "USD" 0
          , aptAmountBilling    = Currency "USD" 0
          , aptVersion          = "1.0"
          , aptAdjustments      = []
          }
      someJson <- readFile "test/json/apto-transaction-balanceinqiry.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Pending Trx" $ do
      let
        msg = AptoTransaction
          { aptSource           = Apto
          , aptIdempotency      = Nothing
          , aptSourceId         = AptoTransactionId "txn_a"
          , aptCardId           = AptoCardId "crd_62d4c88d3f8ae49a"
          , aptDetails          =
            CardTransaction
              { pcpContext         = "Authorization (MASTERCARD)"
              , pcpIsCardPresent   = False
              , pcpIsOnline        = True
              , pcpIsInternational = False
              , pcpNetwork         = Mastercard
              , pcpIsEMV           = False
              , pcpType            = Nothing
              , pcpDescription = Just "APL* ITUNES.COM/BILL 866-712-7753 CAUSA"
              }
          , aptCardholder       = AptoCardHolder
                                    { apcId        = AptoCardholderId
                                                       "crdhldr_a"
                                    , apcEmail = "austin+test@example.com"
                                    , apcNameFirst = "Austin"
                                    , acpNameLast  = "Smith"
                                    }
          , aptMerchant = CardMerchant { cmiMcc      = MastercardMCC "5735"
                                       , cmiMccDesc  = "Record Stores"
                                       , cmiName     = "APL* ITUNES.COM/BILL"
                                       , cmiLocality = Just "866-712-7753"
                                       , cmiRegion   = Just "CA"
                                       , cmiCountry  = "USA"
                                       }
          , aptStandin          = False
          , aptState            = TrxPending
          , aptCreatedAt        = stringToTime "2019-10-16T11:52:15+00:00"
          , aptTransactionEvent = StateTransition
          , aptAmountLocal = Currency "USD"
                                      ((-4458563631096791) % 4503599627370496)
          , aptAmountHold       = Currency "USD" 0
          , aptAmountCashback   = Currency "USD" 0
          , aptAmountFee        = Currency "USD" 0
          , aptAmountBilling    = Currency
                                    "USD"
                                    ((-4458563631096791) % 4503599627370496)
          , aptVersion          = "1.0"
          , aptAdjustments      =
            [ AptoAdjustment
                { adjId              = AptoAdjustmentID "adj_eff035299144fd25"
                , adjCreatedAt       = stringToTime "2019-10-16T11:52:15Z"
                , adjAmountLocal     = Currency
                                         "USD"
                                         ((-4458563631096791) % 4503599627370496)
                , adjAmountBilling   = Currency
                                         "USD"
                                         ((-4458563631096791) % 4503599627370496)
                , adjFSTransactionId = Just
                                         "00000000-0000-0000-0000-000000000000"
                , adjType            = CaptureAdjustment
                }
            ]
          }
      someJson <- readFile "test/json/apto-transaction-pending.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Complete Trx" $ do
      let
        msg = AptoTransaction
          { aptSource           = Apto
          , aptIdempotency      = Nothing
          , aptSourceId         = AptoTransactionId "txn_a"
          , aptCardId           = AptoCardId "crd_62d4c88d3f8ae49a"
          , aptDetails          =
            CardTransaction
              { pcpContext         = "Standard purchase (MASTERCARD)"
              , pcpIsCardPresent   = False
              , pcpIsOnline        = True
              , pcpIsInternational = False
              , pcpNetwork         = Mastercard
              , pcpIsEMV           = False
              , pcpType            = Just Signature
              , pcpDescription = Just "APL* ITUNES.COM/BILL 866-712-7753 CAUSA"
              }
          , aptCardholder       = AptoCardHolder
                                    { apcId        = AptoCardholderId
                                                       "crdhldr_a"
                                    , apcEmail = "austin+test@example.com"
                                    , apcNameFirst = "Austin"
                                    , acpNameLast  = "Smith"
                                    }
          , aptMerchant = CardMerchant { cmiMcc      = MastercardMCC "5735"
                                       , cmiMccDesc  = "Record Stores"
                                       , cmiName     = "APL* ITUNES.COM/BILL"
                                       , cmiLocality = Just "866-712-7753"
                                       , cmiRegion   = Just "CA"
                                       , cmiCountry  = "USA"
                                       }
          , aptStandin          = False
          , aptState            = TrxCompleted
          , aptCreatedAt        = stringToTime "2019-10-16T11:52:15+00:00"
          , aptTransactionEvent = StateTransition
          , aptAmountLocal = Currency "USD"
                                      ((-4458563631096791) % 4503599627370496)
          , aptAmountHold       = Currency "USD" 0
          , aptAmountCashback   = Currency "USD" 0
          , aptAmountFee        = Currency "USD" 0
          , aptAmountBilling    = Currency
                                    "USD"
                                    ((-4458563631096791) % 4503599627370496)
          , aptVersion          = "1.0"
          , aptAdjustments      =
            [ AptoAdjustment
              { adjId              = AptoAdjustmentID "adj_159843e36fdd627f"
              , adjCreatedAt       = stringToTime "2019-10-18T08:52:11Z"
              , adjAmountLocal     = Currency "USD" (0 % 1)
              , adjAmountBilling   = Currency "USD" (0 % 1)
              , adjFSTransactionId = Just "00000000-0000-0000-0000-000000000000"
              , adjType            = RefundAdjustment
              }
            , AptoAdjustment
              { adjId              = AptoAdjustmentID "adj_eff035299144fd25"
              , adjCreatedAt       = stringToTime "2019-10-16T11:52:15Z"
              , adjAmountLocal     = Currency
                                       "USD"
                                       ((-4458563631096791) % 4503599627370496)
              , adjAmountBilling   = Currency
                                       "USD"
                                       ((-4458563631096791) % 4503599627370496)
              , adjFSTransactionId = Just "00000000-0000-0000-0000-000000000000"
              , adjType            = CaptureAdjustment
              }
            ]
          }
      someJson <- readFile "test/json/apto-transaction-complete.json"
      A.eitherDecode someJson `shouldBe` Right msg

    -- has a refund adjustment
    it "Complete Trx with refund adjustment" $ do
      let
        msg = AptoTransaction
          { aptSource           = Apto
          , aptIdempotency      = Nothing
          , aptSourceId         = AptoTransactionId "txn_a"
          , aptCardId           = AptoCardId "crd_f3f61a9d614db542"
          , aptDetails          =
            CardTransaction
              { pcpContext         = "Standard purchase (MASTERCARD)"
              , pcpIsCardPresent   = False
              , pcpIsOnline        = False
              , pcpIsInternational = False
              , pcpNetwork         = Mastercard
              , pcpIsEMV           = False
              , pcpType            = Just Signature
              , pcpDescription     = Just "T&R SERVICE SAN FRANCISCOCAUSA"
              }
          , aptCardholder       = AptoCardHolder
                                    { apcId        = AptoCardholderId
                                                       "crdhldr_a"
                                    , apcEmail     = "adam@example.com"
                                    , apcNameFirst = "Adam"
                                    , acpNameLast  = "Juhasz"
                                    }
          , aptMerchant         = CardMerchant
                                    { cmiMcc      = MastercardMCC "5542"
                                    , cmiMccDesc  = "Automated Fuel Dispensers"
                                    , cmiName     = "T&R SERVICE"
                                    , cmiLocality = Just "SAN FRANCISCO"
                                    , cmiRegion   = Just "CA"
                                    , cmiCountry  = "USA"
                                    }
          , aptStandin          = False
          , aptState            = TrxCompleted
          , aptCreatedAt        = stringToTime "2019-11-23T19:23:36+00:00"
          , aptTransactionEvent = StateTransition
          , aptAmountLocal      = Currency "USD" ((-91) % 1)
          , aptAmountHold       = Currency "USD" 0
          , aptAmountCashback   = Currency "USD" 0
          , aptAmountFee        = Currency "USD" 0
          , aptAmountBilling    = Currency
                                    "USD"
                                    ((-5561945539802563) % 281474976710656)
          , aptVersion          = "1.0"
          , aptAdjustments      =
            [ AptoAdjustment
              { adjId              = AptoAdjustmentID "adj_e7a2596eb1e87daf"
              , adjCreatedAt       = stringToTime "2019-11-23T19:24:50Z"
              , adjAmountLocal     = Currency "USD"
                                              (643170321783849 % 4398046511104)
              , adjAmountBilling   = Currency "USD"
                                              (643170321783849 % 4398046511104)
              , adjFSTransactionId = Nothing
              , adjType            = RefundAdjustment
              }
            , AptoAdjustment
              { adjId              = AptoAdjustmentID "adj_d4847f8b25d436c1"
              , adjCreatedAt       = stringToTime "2019-11-23T19:23:36Z"
              , adjAmountLocal     = Currency "USD" ((-166) % 1)
              , adjAmountBilling   = Currency "USD" ((-166) % 1)
              , adjFSTransactionId = Just "00000000-0000-0000-0000-000000000000"
              , adjType            = CaptureAdjustment
              }
            ]
          }
      someJson <- readFile "test/json/apto-transaction-complete-adj-refund.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "AVS transaction" $ do
    -- This is an AVS transaction. AVS is a type of transaction that some 
    -- merchants initiate to certify cardholder's information like address, 
    -- CVV, etc. If AVS is successful, then they initiate the actual transaction. 
    -- The testing that you see here was done by myself and it could have been 
    -- the fact that I was testing decline due to wrong CVV.
      let msg = CardNotification
            { notSource     = Apto
            , notSourceId   = AptoTransactionId "txn_a"
            , notCardId     = AptoCardId "crd_b8568d3ae9725aa3"
            , notCardholder = AptoCardHolder
                                { apcId        = AptoCardholderId
                                                   "crdhldr_a"
                                , apcEmail     = "kendra@example.com"
                                , apcNameFirst = "Kendra"
                                , acpNameLast  = "Barker"
                                }
            , notContext    = "Uncategorized (MASTERCARD)"
            }

      someJson <- readFile
        "test/json/apto-transaction-complete-unsupported.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Apple pay notification" $ do
      let msg = CardNotification
            { notSource     = Apto
            , notSourceId   = AptoTransactionId "txn_a"
            , notCardId     = AptoCardId "crd_b12016953c5a7323"
            , notCardholder = AptoCardHolder
                                { apcId        = AptoCardholderId
                                                   "crdhldr_a"
                                , apcEmail     = "<removed>"
                                , apcNameFirst = "<removed>"
                                , acpNameLast  = "<removed>"
                                }
            , notContext    = "MASTERCARD Token: <removed> ()"
            }
      someJson <- readFile "test/json/apto-transacton-applepay.json"
      A.eitherDecode someJson `shouldBe` Right msg


    it "nonfinancial" $ do
      let msg = CardNotification
            { notSource     = Apto
            , notSourceId   = AptoTransactionId "txn_a"
            , notCardId     = AptoCardId "crd_b12016953c5a7323"
            , notCardholder = AptoCardHolder
                                { apcId        = AptoCardholderId
                                                   "crdhldr_a"
                                , apcEmail     = "<removed>"
                                , apcNameFirst = "<removed>"
                                , acpNameLast  = "<removed>"
                                }
            , notContext    = "Uncategorized (MASTERCARD)"
            }
      someJson <- readFile "test/json/apto-nonfinancial.json"
      A.eitherDecode someJson `shouldBe` Right msg



