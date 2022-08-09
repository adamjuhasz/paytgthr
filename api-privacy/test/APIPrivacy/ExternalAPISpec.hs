{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module APIPrivacy.ExternalAPISpec
  ( spec
  ) where

import           APIPrivacy.ExternalAPI.Models  ( ASAMessage )
import           APIPrivacy.ExternalAPI.Transaction
import           APIPrivacy.Models.Privacy      ( EventType(Return)
                                                , Transaction
                                                  ( Transaction
                                                  , amount
                                                  , events
                                                  )
                                                , TransactionEvent
                                                  ( TransactionEvent
                                                  , amount
                                                  , eventResult
                                                  , eventType
                                                  )
                                                , TransactionResult(Approved)
                                                )
import           Data.Aeson                     ( eitherDecode )
import           Data.ByteString.Lazy           ( readFile )
import           Prelude                 hiding ( readFile )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , AptoAdjustmentType(..)
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldSatisfy
                                                )

spec :: Spec
spec = parallel $ do
  describe "ASA Message" $ do
    it "decodes" $ do
      someJson <- readFile "./test/APIPrivacy/ExternalAPI/privacy-asa.json"
      let decoded :: Either String ASAMessage = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  _ -> False
                        )

  describe "General Decline Webhook" $ do
    it "decodes" $ do
      someJson <- readFile
        "./test/APIPrivacy/ExternalAPI/privacy-webhook-gen-decline.json"
      let decoded :: Either String Transaction = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  _ -> False
                        )

  describe "Malformed response webhook" $ do
    it "decodes" $ do
      someJson <- readFile
        "./test/APIPrivacy/ExternalAPI/privacy-webhook-malformed.json"
      let decoded :: Either String Transaction = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right _ -> True
                          Left  _ -> False
                        )

  describe "Refund trx webhook" $ do
    it "decodes" $ do
      someJson <- readFile
        "./test/APIPrivacy/ExternalAPI/privacy-webhook-refund.json"
      let decoded :: Either String Transaction = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right Transaction { amount = Currency "USD" (-10), events = [TransactionEvent { amount = Currency "USD" (-10), eventResult = Approved, eventType = Return }] }
                            -> True
                          Right _ -> False
                          Left  _ -> False
                        )

      let d = decoded
            >>= \Transaction {..} -> Right $ processPrivacyEvent events

      d
        `shouldSatisfy` (\case
                          Left _ -> False
                          Right [AptoAdjustment { adjAmountLocal = Currency "USD" 10, adjAmountBilling = Currency "USD" 10, adjFSTransactionId = Nothing, adjType = RefundAdjustment }]
                            -> True
                          Right _ -> False
                        )

  describe "void trx webhook" $ do
    it "decodes" $ do
      someJson <- readFile
        "./test/APIPrivacy/ExternalAPI/privacy-webhook-void.json"
      let decoded :: Either String Transaction = eitherDecode someJson
      decoded
        `shouldSatisfy` (\case
                          Right Transaction { amount = Currency "USD" 56.56 }
                            -> True
                          Right _ -> False
                          Left  _ -> False
                        )

      let d =
            decoded >>= \Transaction {..} -> Right $ processPrivacyEvent events

      d
        `shouldSatisfy` (\case
                          Left _ -> False
                          Right [AptoAdjustment { adjType = AuthorizationAdjustment, adjAmountBilling = Currency "USD" (-56.59) }, AptoAdjustment { adjAmountLocal = Currency "USD" (-56.56), adjAmountBilling = Currency "USD" (-56.56), adjFSTransactionId = Nothing, adjType = CaptureAdjustment }]
                            -> True
                          Right _ -> False
                        )
