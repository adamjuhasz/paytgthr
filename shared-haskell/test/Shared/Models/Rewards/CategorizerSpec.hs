{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE NoStrictData #-}

module Shared.Models.Rewards.CategorizerSpec where

import           Data.List.NonEmpty             ( fromList )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids              ( MessageID(..)
                                                , TransactionId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Rewards.Boost    ( BoostMatcher(..) )
import           Shared.Models.Rewards.Categorizer
                                                ( matchBoost )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionEvent(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

spec :: Spec
spec = do
  describe "matchBoost" $ do
    it "tests mcc matchig" $ do
      let matcher = MatchMCC $ MastercardMCC "1234"
      let trx     = baseTransaction { trxMerchant = Just baseMerchant }
      matchBoost trx matcher `shouldBe` True

    it "tests text matching" $ do
      let matcher = MatchText "itunes"
      let trx = baseTransaction { trxMerchant = Just baseMerchant }
      matchBoost trx matcher `shouldBe` True

    it "tests AND matching" $ do
      let trx = baseTransaction { trxMerchant = Just baseMerchant }
      matchBoost
          trx
          ( MatchAND
          $ fromList [MatchMCC $ MastercardMCC "9999", MatchText "itunes"]
          )
        `shouldBe` False

      matchBoost
          trx
          ( MatchAND
          $ fromList [MatchMCC $ MastercardMCC "1234", MatchText "apple"]
          )
        `shouldBe` False

      matchBoost
          trx
          ( MatchAND
          $ fromList [MatchMCC $ MastercardMCC "1234", MatchText "itunes"]
          )
        `shouldBe` True

    it "tests OR matching" $ do
      let trx = baseTransaction { trxMerchant = Just baseMerchant }
      matchBoost
          trx
          ( MatchOR
          $ fromList [MatchMCC $ MastercardMCC "9999", MatchText "itunes"]
          )
        `shouldBe` True

      matchBoost
          trx
          ( MatchOR
          $ fromList [MatchMCC $ MastercardMCC "1234", MatchText "apple"]
          )
        `shouldBe` True

      matchBoost
          trx
          ( MatchOR
          $ fromList [MatchMCC $ MastercardMCC "1234", MatchText "itunes"]
          )
        `shouldBe` True

      matchBoost
          trx
          ( MatchOR
          $ fromList [MatchMCC $ MastercardMCC "9999", MatchText "apple"]
          )
        `shouldBe` False

    it "tests NOT matching" $ do
      let trx = baseTransaction { trxMerchant = Just baseMerchant }
      matchBoost trx (MatchNOT $ MatchMCC $ MastercardMCC "9999")
        `shouldBe` True

baseMerchant :: MerchantInfo
baseMerchant = CardMerchant { cmiMcc      = MastercardMCC "1234"
                            , cmiMccDesc  = "Record Stores"
                            , cmiName     = "APL* ITUNES.COM/BILL"
                            , cmiLocality = Just "866-712-7753"
                            , cmiRegion   = Just "CA"
                            , cmiCountry  = "USA"
                            }

baseTransaction :: Transaction
baseTransaction = Transaction
  { trxId                = TransactionId nil
  , trxRevision          = 1
  , trxVersion           = "1.0"
  , trxMsgSource         = MessageID nil
  , trxState             = TrxCreated
  , trxSource            = Apto
  , trxSourceId          = "trx_1"
  , trxSourceEvent       = AuthRequest
  , trxUserId            = UserID nil
  , trxDisplayAmount     = Currency "USD" 100
  , trxBillingAmounts    = []
  , trxPurchasedAt       = unsafePerformIO Clock.getCurrentTime
  , trxDetails           = Nothing
  , trxGroupId           = Nothing
  , trxSourceIdempotency = Nothing
  , trxSplitAmounts      = []
  , trxMerchant          = Nothing
  , trxDescription       = Nothing
  , trxAdjustments       = []
  , trxRewardId = Nothing
  }
