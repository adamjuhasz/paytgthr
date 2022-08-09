{- HLINT ignore "Redundant do" -}
module App.UtilsSpec where

import           Data.List                      ( sort )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID.V4                   ( nextRandom )
import           PaymentAuth.App.Utils          ( addBalancesWithPendingTrx
                                                , unionAuthResult
                                                )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Group            ( GroupId(..) )
import           Shared.Models.Transaction      ( DeclineReason(..)
                                                , Transaction(..)
                                                , TransactionEvent(AuthRequest)
                                                , TransactionId(..)
                                                , TransactionSource(Apto)
                                                , TransactionState(TrxDeclined)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult(..) )
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

anotherUID :: UserID
{-# NOINLINE anotherUID #-}
anotherUID = unsafePerformIO (UserID <$> nextRandom)

thirdUID :: UserID
{-# NOINLINE thirdUID #-}
thirdUID = unsafePerformIO (UserID <$> nextRandom)

aGroupId :: GroupId
{-# NOINLINE aGroupId #-}
aGroupId = unsafePerformIO (GroupId <$> nextRandom)

aMessageId :: MessageID
{-# NOINLINE aMessageId #-}
aMessageId = unsafePerformIO (MessageID <$> nextRandom)

aTransactionId :: TransactionId
{-# NOINLINE aTransactionId #-}
aTransactionId = unsafePerformIO (TransactionId <$> nextRandom)

aTime :: UTCTime
{-# NOINLINE aTime #-}
aTime = unsafePerformIO getCurrentTime

aTrx :: Transaction
aTrx = Transaction
  { trxId                = aTransactionId
  , trxRevision          = 1
  , trxVersion           = "1.0"
  , trxMsgSource         = aMessageId
  , trxState             = TrxDeclined (BalanceCheckFail [aUserID, anotherUID])
  , trxSource            = Apto
  , trxSourceId          = "trx_1"
  , trxSourceEvent       = AuthRequest
  , trxUserId            = aUserID
  , trxDisplayAmount     = Currency "USD" 100
  , trxBillingAmounts    = []
  , trxPurchasedAt       = aTime
  , trxDetails           = Nothing
  , trxGroupId           = Just (aGroupId, 1)
  , trxSourceIdempotency = Nothing
  , trxSplitAmounts      = [(aUserID, 50), (anotherUID, 50)]
  , trxMerchant          = Nothing
  , trxDescription       = Just "ABC INC"
  , trxAdjustments       = []
  , trxRewardId          = Nothing
  }

spec :: Spec
spec = parallel $ do
  describe "unionAuthResult" $ do
    it "combines good results" $ do
      let balances = [Right (1 :: Int), Right (2 :: Int)]
      unionAuthResult balances `shouldBe` Right [1, 2]

    it "returns 1 bad result" $ do
      let balances =
            [Right (1 :: Int), Left (BalanceRequestFailed aTrx [aUserID])]
      unionAuthResult balances `shouldBe` Left
        (BalanceRequestFailed
          (aTrx { trxState = TrxDeclined (BalanceCheckFail [aUserID]) })
          [aUserID]
        )
      unionAuthResult (reverse balances) `shouldBe` Left
        (BalanceRequestFailed
          (aTrx { trxState = TrxDeclined (BalanceCheckFail [aUserID]) })
          [aUserID]
        )

    it "combines 2 bad results" $ do
      let balances :: [Either AuthResult Int] =
            [ Left (BalanceRequestFailed aTrx [aUserID])
            , Left (BalanceRequestFailed aTrx [anotherUID])
            ]
      unionAuthResult balances
        `shouldBe` Left (BalanceRequestFailed aTrx [aUserID, anotherUID])
      unionAuthResult (reverse balances) `shouldBe` Left
        (BalanceRequestFailed
          (aTrx
            { trxState = TrxDeclined (BalanceCheckFail [anotherUID, aUserID])
            }
          )
          [anotherUID, aUserID]
        )
  describe "addBalancesWithPendingTrx" $ do
    it "adds just 2 trxs" $ do
      let balances = addBalancesWithPendingTrx
            [ aTrx { trxDisplayAmount = Currency "USD" 100.01 }
            , aTrx { trxDisplayAmount = Currency "USD" 100 }
            ]
            []
      balances
        `shouldBe` sort
                     [ (aUserID   , Currency "USD" (-100))
                     , (anotherUID, Currency "USD" (-100.01))
                     ]

    it "adds just 2 trxs with 3 balances" $ do
      let balances = addBalancesWithPendingTrx
            [ aTrx { trxDisplayAmount = Currency "USD" 100.01 }
            , aTrx { trxDisplayAmount = Currency "USD" 100 }
            ]
            [ (aUserID   , Currency "USD" (-3.12))
            , (anotherUID, Currency "USD" 0)
            , (thirdUID  , Currency "USD" (-11.23))
            ]
      balances `shouldBe` sort
        [ (aUserID   , Currency "USD" (-103.12))
        , (anotherUID, Currency "USD" (-100.01))
        , (thirdUID  , Currency "USD" (-11.23))
        ]
