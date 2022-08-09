{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module App.Ledger.PureSpec where

import           Data.Ratio                     ( (%) )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( nil )
import           PaymentAuth.App.Ledger.CreateFacts
                                                ( createAdjFacts )
import           Shared.Models.Currency         ( Currency(..)
                                                , getMonetaryValue
                                                , roundUpUSD
                                                )
import           Shared.Models.Ids              ( JournalId(..)
                                                , LedgerEntryId(LedgerEntryId)
                                                , MessageID(..)
                                                , PaymentId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..)
                                                , factAmount
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..)
                                                , defaultEntry
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentMethod(..)
                                                , PaymentStatus(PaymentCreated)
                                                , PaymentSubType(NormalPayment)
                                                , PaymentType(..)
                                                )
import           Shared.Models.Transaction      ( AptoAdjustment
                                                  ( AptoAdjustment
                                                  , adjAmountBilling
                                                  , adjAmountLocal
                                                  , adjCreatedAt
                                                  , adjFSTransactionId
                                                  , adjId
                                                  , adjType
                                                  )
                                                , AptoAdjustmentID
                                                  ( AptoAdjustmentID
                                                  )
                                                , AptoAdjustmentType
                                                  ( CaptureAdjustment
                                                  )
                                                , TransactionId(..)
                                                )
import           Shared.Utils                   ( stringToTime )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

user1 :: UserID
user1 = UserID nil

aMsgId :: MessageID
aMsgId = MessageID nil

aTrxId :: TransactionId
aTrxId = TransactionId nil

aPayId :: PaymentId
aPayId = PaymentId nil

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:15:32+00:00"

debitPayment :: Payment
debitPayment = Payment
  { payId          = PaymentId nil
  , payRevision    = 1
  , payVersion     = "1.0"
  , payMsgSource   = aMsgId
  , payStatus      = PaymentCreated
  , payUser        = user1
  , payType        = DebitFromUser
  , payMethod      = DwollaSettlement
  , payMethodId    = Nothing
  , payAmount      = Currency "USD" 10
  , payText        = "Tgthr Card"
  , paySubType     = NormalPayment
  , payACHInfo     = Nothing
  , payCreatedAt   = stringToTime "2019-11-01T20:15:32+00:00"
  , payFromJournal = Nothing
  , payToJournal   = Nothing
  }

addEntry :: LedgerFact -> LedgerEntry -> LedgerEntry
addEntry fact entry@LedgerEntry {..} = entry
  { lenFact    = fact
  , lenBalance = roundUpUSD (lenBalance + amount)
  }
  where amount = factAmount fact

spec :: Spec
spec = parallel $ do
  describe "Ledger Demo" $ do
    it "payments taking multiple days" $ do
      -- Day 1
      let iceCream = TrxAdjustment aTrxId (Currency "USD" (-10))
          bowling  = TrxAdjustment aTrxId (Currency "USD" (-50))

      -- Day 2
      -- Payment sweep run
          -- payment initiated (60)
          rent     = TrxAdjustment aTrxId (Currency "USD" (-500))

      -- Day 3
          --payment initated (500)

      -- Day 4
      -- Day 5
          cleared1 = PaymentCleared aPayId (Currency "USD" 60)

      -- Day 6
          cleared2 = PaymentCleared aPayId (Currency "USD" 500)


      let step1 = defaultEntry user1
                               aMsgId
                               Nothing
                               iceCream
                               aTime
                               (LedgerEntryId nil)
                               (JournalId nil)
          step2 = addEntry bowling step1
          step4 = addEntry rent step2
          step6 = addEntry cleared1 step4
          step7 = addEntry cleared2 step6

      lenBalance step1 `shouldBe` Currency "USD" (-10)
      lenBalance step2 `shouldBe` Currency "USD" (-60)
      lenBalance step4 `shouldBe` Currency "USD" (-560)
      lenBalance step6 `shouldBe` Currency "USD" (-500)
      lenBalance step7 `shouldBe` Currency "USD" 0

  describe "createAdjFacts" $ do
    it "splits -3.99" $ do
      let
        trxInfo = (TransactionId nil, [(UserID nil, 50), (UserID nil, 50)])
        adj1    = AptoAdjustment
          { adjId              = AptoAdjustmentID "adj_eff035299144fd25D"
          , adjCreatedAt       = stringToTime "2019-10-16T11:52:15+00:00"
          , adjAmountLocal = Currency "USD"
                                      ((-3372070220993659) % 562949953421312)
          , adjAmountBilling   = Currency
                                   "USD"
                                   ((-2246170314151035) % 562949953421312)
          , adjFSTransactionId = Just "00000000-0000-0000-0000-000000000000"
          , adjType            = CaptureAdjustment
          }
        facts = createAdjFacts trxInfo [adj1]
      facts
        `shouldBe` [ ( UserID nil
                     , Just "adj_eff035299144fd25D"
                     , TrxAdjustment (TransactionId nil) (Currency "USD" (-2))
                     )
                   , ( UserID nil
                     , Just "adj_eff035299144fd25D"
                     , TrxAdjustment (TransactionId nil)
                                     (Currency "USD" (-1.99))
                     )
                   ]
      let testValue :: Currency -> Double
          testValue = fromRational . getMonetaryValue
          sumCurr   = foldr (+) (Currency "USD" 0)
          getFact (_, _, f) = factAmount f
      testValue (sumCurr (fmap getFact facts))
        `shouldBe` testValue (adjAmountBilling adj1)
