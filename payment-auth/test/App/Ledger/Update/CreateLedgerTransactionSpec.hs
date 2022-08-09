{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.Ledger.Update.CreateLedgerTransactionSpec where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( nil )
import           Data.UUID.V4                   ( nextRandom )
import           PaymentAuth.App.Ledger.Update  ( createLedgerTransaction )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.Ids              ( JournalId(JournalId)
                                                , LedgerEntryId(LedgerEntryId)
                                                , LedgerTrxId(LedgerTrxId)
                                                , MessageID(MessageID)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalType(VirtualAccount)
                                                , LedgerJournal(..)
                                                )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction(..)
                                                , TransactionEntries(..)
                                                , verifyTransactionIsSound
                                                )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( midToTrace )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

mid :: MessageID
mid = MessageID nil

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:15:32+00:00"

journal1 :: JournalId
{-# NOINLINE journal1 #-}
journal1 = unsafePerformIO (JournalId <$> nextRandom)

journal2 :: JournalId
{-# NOINLINE journal2 #-}
journal2 = unsafePerformIO (JournalId <$> nextRandom)

entry1 :: LedgerEntryId
{-# NOINLINE entry1 #-}
entry1 = unsafePerformIO (LedgerEntryId <$> nextRandom)

entry2 :: LedgerEntryId
{-# NOINLINE entry2 #-}
entry2 = unsafePerformIO (LedgerEntryId <$> nextRandom)

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO, MonadCatch
      , MonadThrow
      , MonadMask
      )

instance HasTime TestMonad where
  getCurrentTime = return aTime

aJournal :: LedgerJournal
aJournal = LedgerJournal { journalId             = journal1
                         , journalType           = VirtualAccount
                         , journalName           = "journal"
                         , journalUser           = Nothing
                         , lastJournalEntry      = entry1
                         , journalBalance        = Currency "USD" 0
                         , journalPendingBalance = Currency "USD" 0
                         , journalRevision       = 1
                         , journalCreated        = aTime
                         , journalUpdated        = aTime
                         , journalTransaction    = LedgerTrxId nil
                         }

anEntry :: LedgerEntry
anEntry = LedgerEntry { lenId             = entry1
                      , lenJournal        = journal1
                      , lenUser           = Nothing
                      , lenRevision       = 1
                      , lenVersion        = "1.0"
                      , lenMsgSource      = mid
                      , lenBalance        = Currency "USD" 0
                      , lenPendingBalance = Currency "USD" 0
                      , lenFact           = Manual $ Currency "USD" 0
                      , lenIdempotency    = Nothing
                      , lenCreatedAt      = aTime
                      , lenTransaction    = Nothing
                      }

instance HasLedgerDB TestMonad where
  getLedgerJournal _ jid
    | jid == journal1 = return
    $ Just aJournal { journalId = journal1, lastJournalEntry = entry1 }
    | jid == journal2 = return
    $ Just aJournal { journalId = journal2, lastJournalEntry = entry2 }
    | otherwise = return Nothing
  getLedgerEntry _ leid
    | leid == entry1 = return
    $ Just anEntry { lenId = entry1, lenJournal = journal1 }
    | leid == entry2 = return
    $ Just anEntry { lenId = entry2, lenJournal = journal2 }
    | otherwise = return Nothing
  saveLedgerTransaction _ trx _ = do
    let verified = verifyTransactionIsSound trx

    _ <- case verified of
      Right _ -> return ()
      Left  t -> error t

    return ()
  getLedgerJournalType _ _ = return [aJournal { journalId = JournalId nil }]

instance HasRandom TestMonad where
  aRandomUUID = return nil

spec :: Spec
spec = parallel $ do
  describe "createLedgerTransaction" $ do
    it "happy path" $ do
      trace                  <- midToTrace mid
      LedgerTransaction {..} <- unTestMonad $ createLedgerTransaction
        trace
        Nothing
        journal1
        journal2
        (Manual $ Currency "USD" 30)
      let entries = case ltxEntries of
            Entries     x -> x
            EntriesById _ -> error "EntriesById"
      let j1Entry = case filter ((==) journal1 . lenJournal) entries of
            []    -> error "emptu"
            [ x ] -> x
            _ : _ -> error "too many"
      lenBalance j1Entry `shouldBe` Currency "USD" (-30)
