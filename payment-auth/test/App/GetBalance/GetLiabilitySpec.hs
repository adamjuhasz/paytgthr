{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.GetBalance.GetLiabilitySpec
  ( spec
  ) where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( nil )
import           PaymentAuth.App.GetBalance     ( getLiability )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Payments           ( paymentAmount
                                                , paymentCredit
                                                , paymentDebit
                                                )
import           Scaffolding.Transactions       ( asos
                                                , asosAmount
                                                )
import           Scaffolding.Users              ( userJohn )
import           Shared.Models.Currency         ( Currency(..)
                                                , roundDownUSD
                                                )
import           Shared.Models.Ids
import           Shared.Models.Ledger.Common    ( LedgerFact(Manual) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

newtype LedgerAndCredit a =
    LedgerAndCredit { unLedgerAndCredit :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:18:14.284+00:00"

entry :: LedgerEntry
entry = LedgerEntry { lenId             = LedgerEntryId nil
                    , lenUser           = Just $ usrUserID userJohn
                    , lenRevision       = 1
                    , lenVersion        = "1.0"
                    , lenMsgSource      = MessageID nil
                    , lenBalance        = paymentAmount
                    , lenFact           = Manual (Currency "USD" 0)
                    , lenIdempotency    = Nothing
                    , lenCreatedAt      = aTime
                    , lenJournal        = JournalId nil
                    , lenPendingBalance = Currency "USD" 0
                    , lenTransaction    = Nothing
                    }

instance HasLedgerDB LedgerAndCredit where
  getLedgerJournalType _ (GetPayTgthr u)
    | u == usrUserID userJohn = return
      [ LedgerJournal { journalId             = JournalId nil
                      , journalType           = VirtualAccount
                      , journalName           = ""
                      , journalUser           = Nothing
                      , lastJournalEntry      = lenId entry
                      , journalBalance        = lenBalance entry
                      , journalPendingBalance = lenPendingBalance entry
                      , journalRevision       = 1
                      , journalCreated        = aTime
                      , journalUpdated        = aTime
                      , journalTransaction    = LedgerTrxId nil
                      }
      ]
    | otherwise = return []
  getLedgerJournalType _ _ = return []
instance HasTransactionsDB LedgerAndCredit where
  getPendingTransactionsFor _ _ = return []
instance HasPaymentsDB LedgerAndCredit where
  getPendingPaymentsOf _ _ _ = return [paymentCredit]


newtype Combo a =
    Combo { unCombo :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )

entry2 :: LedgerEntry
entry2 = LedgerEntry { lenId             = LedgerEntryId nil
                     , lenUser           = Just $ usrUserID userJohn
                     , lenRevision       = 1
                     , lenVersion        = "1.0"
                     , lenMsgSource      = MessageID nil
                     , lenBalance        = Currency "USD" (-34)
                     , lenFact           = Manual (Currency "USD" (-34))
                     , lenIdempotency    = Nothing
                     , lenCreatedAt      = aTime
                     , lenJournal        = JournalId nil
                     , lenPendingBalance = Currency "USD" 0
                     , lenTransaction    = Nothing
                     }

instance HasLedgerDB Combo where
  getLedgerJournalType _ (GetPayTgthr _) = return
    [ LedgerJournal { journalId             = JournalId nil
                    , journalType           = VirtualAccount
                    , journalName           = ""
                    , journalUser           = Nothing
                    , lastJournalEntry      = lenId entry2
                    , journalBalance        = lenBalance entry2
                    , journalPendingBalance = lenPendingBalance entry2
                    , journalRevision       = 1
                    , journalCreated        = aTime
                    , journalUpdated        = aTime
                    , journalTransaction    = LedgerTrxId nil
                    }
    ]
  getLedgerJournalType _ _ = return []
instance HasTransactionsDB Combo where
  getPendingTransactionsFor _ _ = return [asos]
instance HasPaymentsDB Combo where
  getPendingPaymentsOf _ _ _ = return [paymentDebit]

spec :: Spec
spec = parallel $ do
  describe "getLiability" $ do
    let ledgerAmount = Currency "USD" (-34)

    it "tests credit payment" $ do
      trace <- randomTrace
      val   <- unLedgerAndCredit $ getLiability trace (usrUserID userJohn)
      val `shouldBe` Currency "USD" 0

    it "tests combo" $ do
      trace <- randomTrace
      val   <- unCombo $ getLiability trace (usrUserID userJohn)
      val `shouldBe` (ledgerAmount - roundDownUSD (asosAmount * 0.6))
