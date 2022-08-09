{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Models.AptoPaymentSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile )
import           Test.Hspec
import           Data.Aeson
import           Data.ByteString.Lazy           ( readFile )
import           Data.Text                      ( pack )
import           Data.Ratio
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.Currency
import           Shared.Models.Transaction
import           Shared.Models.Apto.Base
import           Shared.Models.Apto.Transaction
import           Shared.Models.Card

import           Shared.Utils

instance IsString UTCTime where
  fromString = stringToTime

instance IsString AptoCardholderId where
  fromString = AptoCardholderId . pack

spec :: Spec
spec = parallel $ do
  describe "Transaction Webhook" $ it "Declined" $ do
    someJson <- readFile
      "test/json/messages/apto-transaction-declined-insufficent.json"
    let
      expected = AptoTransaction
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
            , pcpDescription = Just "APL* ITUNES.COM/BILL 866-712-7753 CAUSA"
            , pcpType            = Nothing
            }
        , aptCardholder       = AptoCardHolder
                                  { apcId = "crdhldr_a"
                                  , apcEmail     = "nikki+test@example.com"
                                  , apcNameFirst = "Nicole"
                                  , acpNameLast  = "Wong"
                                  }
        , aptMerchant         = CardMerchant { cmiMcc = MastercardMCC "5735"
                                             , cmiMccDesc = "Record Stores"
                                             , cmiName = "APL* ITUNES.COM/BILL"
                                             , cmiLocality = Just "866-712-7753"
                                             , cmiRegion = Just "CA"
                                             , cmiCountry = "USA"
                                             }
        , aptStandin          = False
        , aptState            = TrxDeclined (LowBalance [])
        , aptTransactionEvent = StateTransition
        , aptAmountLocal      = Currency "USD" (-2.0)
        , aptAmountHold       = Currency "USD" 0.0
        , aptAmountCashback   = Currency "USD" 0.0
        , aptAmountFee        = Currency "USD" 0.0
        , aptVersion          = "1.0"
        , aptCreatedAt        = stringToTime "2019-10-04T21:35:11+00:00"
        , aptAmountBilling    = Currency "USD" 0
        , aptAdjustments      = []
        }
    eitherDecode someJson `shouldBe` Right expected

  describe "Transaction Webhook" $ it "Pending" $ do
    someJson <- readFile "test/json/messages/apto-transaction-pending.json"
    let
      expected = AptoTransaction
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
                                  , pcpDescription     = Nothing
                                  , pcpType            = Just Pin
                                  }
        , aptCardholder = AptoCardHolder { apcId = "crdhldr_a"
                                         , apcEmail     = "pat@example.com"
                                         , apcNameFirst = "Josh"
                                         , acpNameLast  = "Wilson"
                                         }
        , aptMerchant         = CardMerchant { cmiMcc = MastercardMCC "7299"
                                             , cmiMccDesc = "Food and Beverage"
                                             , cmiName = "LORDS PHARMACY LTD"
                                             , cmiLocality = Just "LONDON"
                                             , cmiRegion = Nothing
                                             , cmiCountry = "GBR"
                                             }
        , aptStandin          = False
        , aptState            = TrxPending
        , aptTransactionEvent = StateTransition
        , aptAmountLocal      = Currency "USD"
                                         ((-3317182600535081) % 140737488355328)
        , aptAmountHold       = Currency "USD" 0.0
        , aptAmountCashback   = Currency "USD" 0.0
        , aptAmountFee        = Currency "USD" 2.5
        , aptVersion          = "1.0"
        , aptCreatedAt        = stringToTime "2016-10-19T23:25:17+00:00"
        , aptAmountBilling    = Currency "USD"
                                         ((-3317182600535081) % 140737488355328)
        , aptAdjustments      =
          [ AptoAdjustment
              { adjId              = AptoAdjustmentID "adj_3bc9dcf557975fe4"
              , adjCreatedAt       = stringToTime "2018-07-04T15:15:51+00:00"
              , adjAmountLocal     = Currency
                                       "USD"
                                       ((-3317182600535081) % 140737488355328)
              , adjAmountBilling   = Currency
                                       "USD"
                                       ((-3317182600535081) % 140737488355328)
              , adjFSTransactionId = Just "00000000-0000-0000-0000-000000000000"
              , adjType            = CaptureAdjustment
              }
          ]
        }
    eitherDecode someJson `shouldBe` Right expected
