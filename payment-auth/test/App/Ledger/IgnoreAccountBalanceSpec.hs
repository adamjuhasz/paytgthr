{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods -fno-warn-incomplete-uni-patterns #-}

module App.Ledger.IgnoreAccountBalanceSpec where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Ratio                     ( (%) )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( nil )
import           PaymentAuth.App.Ledger.SchedulePayments
                                                ( schedulePaymentsFromLedger )
import           PaymentAuth.App.Payments.Effects
                                                ( PaymentEffects(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids              ( JournalId(JournalId)
                                                , LedgerEntryId(LedgerEntryId)
                                                , LedgerTrxId(LedgerTrxId)
                                                , MessageID(..)
                                                , PaymentId(..)
                                                , TransactionId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Ledger.Journal   ( JournalSearch
                                                  ( GetFundingSource
                                                  , GetPayTgthr
                                                  )
                                                , JournalType(VirtualAccount)
                                                , LedgerJournal(..)
                                                , PaymentMethod
                                                  ( DwollaSettlement
                                                  )
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentStatus(PaymentCreated)
                                                , PaymentSubType(NormalPayment)
                                                , PaymentType(DebitFromUser)
                                                )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( randomTrace )
import           Test.Hspec                     ( Spec
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

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )

instance HasLedgerDB TestMonad where
  getUsersWithBalances _ = return [(user1, Currency "USD" (-50))]
  getLedgerJournalType _ (GetPayTgthr _) = return
    [ LedgerJournal { journalId             = JournalId nil
                    , journalType           = VirtualAccount
                    , journalName           = "journal"
                    , journalUser           = Nothing
                    , lastJournalEntry      = LedgerEntryId nil
                    , journalBalance        = Currency "USD" (-50)
                    , journalPendingBalance = Currency "USD" 0
                    , journalRevision       = 1
                    , journalCreated        = aTime
                    , journalUpdated        = aTime
                    , journalTransaction    = LedgerTrxId nil
                    }
    ]
  getLedgerJournalType _ (GetFundingSource _) = return
    [ LedgerJournal { journalId             = JournalId nil
                    , journalType           = VirtualAccount
                    , journalName           = "journal"
                    , journalUser           = Nothing
                    , lastJournalEntry      = LedgerEntryId nil
                    , journalBalance        = Currency "USD" 0
                    , journalPendingBalance = Currency "USD" 0
                    , journalRevision       = 1
                    , journalCreated        = aTime
                    , journalUpdated        = aTime
                    , journalTransaction    = LedgerTrxId nil
                    }
    ]
  getLedgerJournalType _ x = error $ show x
instance HasRandom TestMonad where
  aRandomUUID = return nil
instance HasPaymentsDB TestMonad where
  savePayment _ _ = return ()
  getPendingPaymentsOf _ theUser _ | theUser == user1 = return []
                                   | otherwise = error "What user is this?"
instance HasTime TestMonad where
  getCurrentTime = return $ stringToTime "2019-11-01T20:15:32+00:00"

spec :: Spec
spec = parallel $ do
  it "ignores account balance" $ do
    trace    <- randomTrace
    payments <- unTestMonad $ schedulePaymentsFromLedger trace
    fmap (\(PaymentWasCreated Payment {..}) -> (payType, payAmount)) payments
      `shouldBe` [(DebitFromUser, Currency "USD" (50 % 1))]
