{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plaid.WebhookTypesSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile )
import           Test.Hspec
import           Data.Text                      ( pack )
import           Data.String                    ( IsString
                                                , fromString
                                                )
import           Data.Aeson
import           Shared.Models.Plaid.Webhook
import           Data.ByteString.Lazy           ( readFile )
import           Shared.Models.Plaid.Base

instance IsString ItemId where
  fromString = ItemId . pack

spec :: Spec
spec = parallel $ do
  describe "AUTH" $ do
    it "AUTOMATICALLY_VERIFIED" $ do
      let
        plaidjson
          = "{\"webhook_type\": \"AUTH\",\"webhook_code\": \"AUTOMATICALLY_VERIFIED\",\"item_id\": \"123\",\"account_id\": \"xyz\"}"
      jsoncontents <- readFile "test/json/AUTOMATICALLY_VERIFIED.json"

      eitherDecode plaidjson
        `shouldBe` (Right $ Auth $ AutomaticallyVerified (ItemId "123") "xyz")
      eitherDecode jsoncontents
        `shouldBe` (Right $ Auth $ AutomaticallyVerified "123" "xyz")

    it "VERIFICATION_EXPIRED" $ do
      let
        plaidjson
          = "{ \"webhook_type\": \"AUTH\", \"webhook_code\": \"ERROR\", \"item_id\": \"123\", \"account_id\": \"xyz\", \"error\": { \"display_message\": \"String\", \"error_code\": \"String\", \"error_message\": \"String\", \"error_type\": \"VERIFICATION_EXPIRED\" }}"
      eitherDecode plaidjson
        `shouldBe` (Right $ Auth $ VerificationExpired "123" "xyz" $ object
                     [ ("error_type"     , "VERIFICATION_EXPIRED")
                     , ("error_code"     , "String")
                     , ("display_message", "String")
                     , ("error_message"  , "String")
                     ]
                   )
    it "Unknown" $ do
      let plaidjson = "{\"webhook_type\": \"AUTH\",\"webhook_code\": \"NEW\"}"
      eitherDecode plaidjson
        `shouldBe` (Right $ Auth $ UnknownAuth $ object
                     [("webhook_type", "AUTH"), ("webhook_code", "NEW")]
                   )
  describe "TRANSACTIONS" $ do
    it "INITIAL_UPDATE" $ do
      plaidjson <- readFile "test/json/INITIAL_UPDATE.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ Transactions $ InitialUpdate
                     "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                     19
                   )

    it "HISTORICAL_UPDATE" $ do
      plaidjson <- readFile "test/json/HISTORICAL_UPDATE.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ Transactions $ HistoricalUpdate
                     "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                     231
                   )

    it "DEFAULT_UPDATE" $ do
      plaidjson <- readFile "test/json/DEFAULT_UPDATE.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ Transactions $ DefaultUpdate
                     "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                     3
                   )

    it "TRANSACTIONS_REMOVED" $ do
      plaidjson <- readFile "test/json/TRANSACTIONS_REMOVED.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ Transactions $ TransactionsRemoved
                     "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                     [ "yBVBEwrPyJs8GvR77N7QTxnGg6wG74H7dEDN6"
                     , "kgygNvAVPzSX9KkddNdWHaVGRVex1MHm3k9no"
                     ]
                   )

  describe "ITEM" $ do
    it "WEBHOOK_UPDATE_ACKNOWLEDGED" $ do
      plaidjson <- readFile "test/json/WEBHOOK_UPDATE_ACKNOWLEDGED.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ ItemWH $ WebhookUpdateAcknowledge
                     "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                     "https://plaid.com/example/webhook"
                   )

    it "ITEM_ERROR" $ do
      plaidjson <- readFile "test/json/ITEM_ERROR.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ ItemWH $ ItemError "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                                               ItemLoginRequired
                   )

  describe "INCOME" $ it "PRODUCT_READY" $ do
    plaidjson <- readFile "test/json/INCOME_PRODUCT_READY.json"
    eitherDecode plaidjson
      `shouldBe` (Right $ Income $ IncomeProductReady
                   "wz666MBjYWTp2PDzzggYhM6oWWmBb"
                 )

  describe "ASSETS" $ do
    it "PRODUCT_READY" $ do
      plaidjson <- readFile "test/json/ASSETS_PRODUCT_READY.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ Assets $ AssetsProductReady "String")

    it "ASSET_REPORT_ERROR" $ do
      plaidjson <- readFile "test/json/ASSET_REPORT_ERROR.json"
      eitherDecode plaidjson
        `shouldBe` (Right $ Assets $ AssetReportError "String")



