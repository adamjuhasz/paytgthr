{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module App.UpdateTrxSpec where

import           Data.Ratio                     ( (%)
                                                , Ratio
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           PaymentAuth.App.Purchases.UpdateTrx
                                                ( UpdatedTrxInfo(..)
                                                , updateTransaction
                                                )
import           Shared.Models.Apto.Transaction ( MerchantInfo(..)
                                                , TransactionDetails(..)
                                                , TransactionSource(Apto)
                                                , TransactionState(..)
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Transaction     as T
                                                ( CardNetwork(Mastercard)
                                                , DeclineReason(..)
                                                , MastercardMCC(MastercardMCC)
                                                , Transaction(..)
                                                , TransactionEvent(..)
                                                , TransactionId(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.Utils                   ( stringToTime )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

aUserID :: UserID
{-# NOINLINE aUserID #-}
aUserID = unsafePerformIO (UserID <$> nextRandom)

aPartnerID :: UserID
{-# NOINLINE aPartnerID #-}
aPartnerID = unsafePerformIO (UserID <$> nextRandom)

aMessageId :: MessageID
{-# NOINLINE aMessageId #-}
aMessageId = unsafePerformIO (MessageID <$> nextRandom)

aTransactionId :: TransactionId
{-# NOINLINE aTransactionId #-}
aTransactionId = unsafePerformIO (TransactionId <$> nextRandom)

almostNow :: UTCTime
{-# NOINLINE almostNow #-}
almostNow = unsafePerformIO getCurrentTime

ninenineCents :: Ratio Integer
ninenineCents = (-4458563631096791) % 4503599627370496

spec :: Spec
spec = parallel $ do
  describe "Pending" $ do
    it "no auth, no group" $ do
      let utcTransaction = aTransactionId
          utcSource      = Apto
          utcIdempotency = Nothing
          utcSourceId    = "trx_1"
          utcDetails     = CardTransaction { pcpContext         = ""
                                           , pcpIsCardPresent   = False
                                           , pcpIsOnline        = True
                                           , pcpIsInternational = False
                                           , pcpNetwork         = Mastercard
                                           , pcpIsEMV           = False
                                           , pcpType            = Nothing
                                           , pcpDescription     = Nothing
                                           }
          utcMerchant = CardMerchant { cmiMcc      = MastercardMCC "1234"
                                     , cmiMccDesc  = ""
                                     , cmiName     = ""
                                     , cmiLocality = Nothing
                                     , cmiRegion   = Nothing
                                     , cmiCountry  = "USA"
                                     }
          utcState            = TrxPending
          utcCreatedAt        = almostNow
          utcTransactionEvent = StateTransition
          utcAmountLocal      = Currency "USD" ninenineCents
          utcAmountHold       = Currency "USD" 0
          utcAmountCashback   = Currency "USD" 0
          utcAmountFee        = Currency "USD" 0
          utcAmountBilling    = Currency "USD" ninenineCents
          utcDescription      = Nothing
          utcAdjustments      = []
          updateInfo          = UpdatedTrxInfo
            { utiTransaction      = utcTransaction
            , utiSource           = utcSource
            , utiIdempotency      = utcIdempotency
            , utiSourceId         = utcSourceId
            , utiDetails          = utcDetails
            , utiMerchant         = utcMerchant
            , utiState            = utcState
            , utiCreatedAt        = utcCreatedAt
            , utiTransactionEvent = utcTransactionEvent
            , utiAmountLocal      = utcAmountLocal
            , utiAmountHold       = utcAmountHold
            , utiAmountCashback   = utcAmountCashback
            , utiAmountFee        = utcAmountFee
            , utiAmountBilling    = utcAmountBilling
            , utiDescription      = utcDescription
            , utiAdjustments      = utcAdjustments
            }

      let
        t = updateTransaction aMessageId
                              updateInfo
                              almostNow
                              aUserID
                              Nothing
                              Nothing
      trxState t `shouldBe` TrxPending

    it "authed" $ do
      let utcTransaction = aTransactionId
          utcSource      = Apto
          utcIdempotency = Nothing
          utcSourceId    = "trx_1"
          utcDetails     = CardTransaction { pcpContext         = ""
                                           , pcpIsCardPresent   = False
                                           , pcpIsOnline        = True
                                           , pcpIsInternational = False
                                           , pcpNetwork         = Mastercard
                                           , pcpIsEMV           = False
                                           , pcpType            = Nothing
                                           , pcpDescription     = Nothing
                                           }
          utcMerchant = CardMerchant { cmiMcc      = MastercardMCC "1234"
                                     , cmiMccDesc  = ""
                                     , cmiName     = ""
                                     , cmiLocality = Nothing
                                     , cmiRegion   = Nothing
                                     , cmiCountry  = "USA"
                                     }
          utcState            = TrxPending
          utcCreatedAt        = almostNow
          utcTransactionEvent = StateTransition
          utcAmountLocal      = Currency "USD" ninenineCents
          utcAmountHold       = Currency "USD" 0
          utcAmountCashback   = Currency "USD" 0
          utcAmountFee        = Currency "USD" 0
          utcAmountBilling    = Currency "USD" ninenineCents
          utcDescription      = Nothing
          utcAdjustments      = []
          updateInfo          = UpdatedTrxInfo
            { utiTransaction      = utcTransaction
            , utiSource           = utcSource
            , utiIdempotency      = utcIdempotency
            , utiSourceId         = utcSourceId
            , utiDetails          = utcDetails
            , utiMerchant         = utcMerchant
            , utiState            = utcState
            , utiCreatedAt        = utcCreatedAt
            , utiTransactionEvent = utcTransactionEvent
            , utiAmountLocal      = utcAmountLocal
            , utiAmountHold       = utcAmountHold
            , utiAmountCashback   = utcAmountCashback
            , utiAmountFee        = utcAmountFee
            , utiAmountBilling    = utcAmountBilling
            , utiDescription      = utcDescription
            , utiAdjustments      = utcAdjustments
            }
          trxId                = aTransactionId
          trxRevision          = 1
          trxVersion           = "1.0"
          trxMsgSource         = aMessageId
          trxState             = TrxAuthorized
          trxSource            = Apto
          trxSourceId          = "trx_1"
          trxSourceEvent       = StateTransition
          trxUserId            = aUserID
          trxDisplayAmount     = Currency "USD" (abs ninenineCents)
          trxBillingAmounts = [(Currency "USD" (abs ninenineCents), almostNow)]
          trxDetails           = Nothing
          trxGroupId           = Nothing
          trxSourceIdempotency = Nothing
          trxSplitAmounts      = [(aPartnerID, 50), (aUserID, 50)]
          trxMerchant          = Nothing
          trxDescription       = Just "MC"
          trxPurchasedAt       = stringToTime "2019-10-04T21:35:11+00:00"
          trxAdjustments       = []
          trxRewardId          = Nothing
          trx                  = Transaction { .. }

      let t = updateTransaction aMessageId
                                updateInfo
                                almostNow
                                aUserID
                                Nothing
                                (Just trx)
      T.trxState t `shouldBe` TrxPending

  describe "Declined" $ do
    it "-> Declined" $ do
      let utcTransaction = aTransactionId
          utcSource      = Apto
          utcIdempotency = Nothing
          utcSourceId    = "trx_1"
          utcDetails     = CardTransaction { pcpContext         = ""
                                           , pcpIsCardPresent   = False
                                           , pcpIsOnline        = True
                                           , pcpIsInternational = False
                                           , pcpNetwork         = Mastercard
                                           , pcpIsEMV           = False
                                           , pcpType            = Nothing
                                           , pcpDescription     = Nothing
                                           }
          utcMerchant = CardMerchant { cmiMcc      = MastercardMCC "1234"
                                     , cmiMccDesc  = ""
                                     , cmiName     = ""
                                     , cmiLocality = Nothing
                                     , cmiRegion   = Nothing
                                     , cmiCountry  = "USA"
                                     }
          utcState            = TrxDeclined (Unknown "test")
          utcCreatedAt        = almostNow
          utcTransactionEvent = StateTransition
          utcAmountLocal      = Currency "USD" ninenineCents
          utcAmountHold       = Currency "USD" 0
          utcAmountCashback   = Currency "USD" 0
          utcAmountFee        = Currency "USD" 0
          utcAmountBilling    = Currency "USD" ninenineCents
          utcDescription      = Nothing
          utcAdjustments      = []
          updateInfo          = UpdatedTrxInfo
            { utiTransaction      = utcTransaction
            , utiSource           = utcSource
            , utiIdempotency      = utcIdempotency
            , utiSourceId         = utcSourceId
            , utiDetails          = utcDetails
            , utiMerchant         = utcMerchant
            , utiState            = utcState
            , utiCreatedAt        = utcCreatedAt
            , utiTransactionEvent = utcTransactionEvent
            , utiAmountLocal      = utcAmountLocal
            , utiAmountHold       = utcAmountHold
            , utiAmountCashback   = utcAmountCashback
            , utiAmountFee        = utcAmountFee
            , utiAmountBilling    = utcAmountBilling
            , utiDescription      = utcDescription
            , utiAdjustments      = utcAdjustments
            }
          trxId                = aTransactionId
          trxRevision          = 1
          trxVersion           = "1.0"
          trxMsgSource         = aMessageId
          trxState             = TrxDeclined (LowBalance [aPartnerID])
          trxSource            = Apto
          trxSourceId          = "trx_1"
          trxSourceEvent       = StateTransition
          trxUserId            = aUserID
          trxDisplayAmount     = Currency "USD" (abs ninenineCents)
          trxBillingAmounts = [(Currency "USD" (abs ninenineCents), almostNow)]
          trxDetails           = Nothing
          trxGroupId           = Nothing
          trxSourceIdempotency = Nothing
          trxSplitAmounts      = [(aPartnerID, 50), (aUserID, 50)]
          trxMerchant          = Nothing
          trxDescription       = Just "MC"
          trxPurchasedAt       = stringToTime "2019-10-04T21:35:11+00:00"
          trxAdjustments       = []
          trxRewardId          = Nothing
          trx                  = Transaction { .. }

      let t = updateTransaction aMessageId
                                updateInfo
                                almostNow
                                aUserID
                                Nothing
                                (Just trx)
      T.trxState t `shouldBe` TrxDeclined (LowBalance [aPartnerID])

  describe "Completed" $ do
    it "no auth, no group" $ do
      let utcTransaction = aTransactionId
          utcSource      = Apto
          utcIdempotency = Nothing
          utcSourceId    = "trx_1"
          utcDetails     = CardTransaction { pcpContext         = ""
                                           , pcpIsCardPresent   = False
                                           , pcpIsOnline        = True
                                           , pcpIsInternational = False
                                           , pcpNetwork         = Mastercard
                                           , pcpIsEMV           = False
                                           , pcpType            = Nothing
                                           , pcpDescription     = Nothing
                                           }
          utcMerchant = CardMerchant { cmiMcc      = MastercardMCC "1234"
                                     , cmiMccDesc  = ""
                                     , cmiName     = ""
                                     , cmiLocality = Nothing
                                     , cmiRegion   = Nothing
                                     , cmiCountry  = "USA"
                                     }
          utcState            = TrxCompleted
          utcCreatedAt        = almostNow
          utcTransactionEvent = StateTransition
          utcAmountLocal      = Currency "USD" ninenineCents
          utcAmountHold       = Currency "USD" 0
          utcAmountCashback   = Currency "USD" 0
          utcAmountFee        = Currency "USD" 0
          utcAmountBilling    = Currency "USD" ninenineCents
          utcDescription      = Nothing
          utcAdjustments      = []
          updateInfo          = UpdatedTrxInfo
            { utiTransaction      = utcTransaction
            , utiSource           = utcSource
            , utiIdempotency      = utcIdempotency
            , utiSourceId         = utcSourceId
            , utiDetails          = utcDetails
            , utiMerchant         = utcMerchant
            , utiState            = utcState
            , utiCreatedAt        = utcCreatedAt
            , utiTransactionEvent = utcTransactionEvent
            , utiAmountLocal      = utcAmountLocal
            , utiAmountHold       = utcAmountHold
            , utiAmountCashback   = utcAmountCashback
            , utiAmountFee        = utcAmountFee
            , utiAmountBilling    = utcAmountBilling
            , utiDescription      = utcDescription
            , utiAdjustments      = utcAdjustments
            }

      let
        t = updateTransaction aMessageId
                              updateInfo
                              almostNow
                              aUserID
                              Nothing
                              Nothing

      trxState t `shouldBe` TrxCompleted

    it "pending" $ do
      let utcTransaction = aTransactionId
          utcSource      = Apto
          utcIdempotency = Nothing
          utcSourceId    = "trx_1"
          utcDetails     = CardTransaction { pcpContext         = ""
                                           , pcpIsCardPresent   = False
                                           , pcpIsOnline        = True
                                           , pcpIsInternational = False
                                           , pcpNetwork         = Mastercard
                                           , pcpIsEMV           = False
                                           , pcpType            = Nothing
                                           , pcpDescription     = Nothing
                                           }
          utcMerchant = CardMerchant { cmiMcc      = MastercardMCC "1234"
                                     , cmiMccDesc  = ""
                                     , cmiName     = ""
                                     , cmiLocality = Nothing
                                     , cmiRegion   = Nothing
                                     , cmiCountry  = "USA"
                                     }
          utcState            = TrxCompleted
          utcCreatedAt        = almostNow
          utcTransactionEvent = StateTransition
          utcAmountLocal      = Currency "USD" ninenineCents
          utcAmountHold       = Currency "USD" 0
          utcAmountCashback   = Currency "USD" 0
          utcAmountFee        = Currency "USD" 0
          utcAmountBilling    = Currency "USD" ninenineCents
          utcDescription      = Nothing
          utcAdjustments      = []
          updateInfo          = UpdatedTrxInfo
            { utiTransaction      = utcTransaction
            , utiSource           = utcSource
            , utiIdempotency      = utcIdempotency
            , utiSourceId         = utcSourceId
            , utiDetails          = utcDetails
            , utiMerchant         = utcMerchant
            , utiState            = utcState
            , utiCreatedAt        = utcCreatedAt
            , utiTransactionEvent = utcTransactionEvent
            , utiAmountLocal      = utcAmountLocal
            , utiAmountHold       = utcAmountHold
            , utiAmountCashback   = utcAmountCashback
            , utiAmountFee        = utcAmountFee
            , utiAmountBilling    = utcAmountBilling
            , utiDescription      = utcDescription
            , utiAdjustments      = utcAdjustments
            }
          trxId                = aTransactionId
          trxRevision          = 2
          trxVersion           = "1.0"
          trxMsgSource         = aMessageId
          trxState             = TrxPending
          trxSource            = Apto
          trxSourceId          = "trx_1"
          trxSourceEvent       = StateTransition
          trxUserId            = aUserID
          trxDisplayAmount     = Currency "USD" (abs ninenineCents)
          trxBillingAmounts    = [(Currency "USD" ninenineCents, almostNow)]
          trxDetails           = Nothing
          trxGroupId           = Nothing
          trxSourceIdempotency = Nothing
          trxSplitAmounts      = [(aPartnerID, 50), (aUserID, 50)]
          trxMerchant          = Nothing
          trxDescription       = Just "MC"
          trxPurchasedAt       = stringToTime "2019-10-04T21:35:11+00:00"
          trxAdjustments       = []
          trxRewardId          = Nothing
          trx                  = Transaction { .. }

      let t = updateTransaction aMessageId
                                updateInfo
                                almostNow
                                aUserID
                                Nothing
                                (Just trx)

      T.trxState t `shouldBe` TrxCompleted
