{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use lambda-case" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Callback.UpdateTransactionSpec
  ( spec
  ) where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Maybe                     ( isJust )
import           Data.Time.Clock
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           PaymentAuth.App.Purchases.UpdateTransaction
                                                ( updateTransaction )
import           PaymentAuth.App.Purchases.UpdateTrx
                                                ( UpdatedTrxInfo(..) )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Groups             ( basicGroup )
import           Scaffolding.Transactions       ( basicTrx )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.Apto.Transaction ( TransactionState(..) )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Group            ( GroupModel(..)
                                                , GroupStatus(..)
                                                )
import           Shared.Models.Ids
import           Shared.Models.Ledger.Journal
import           Shared.Models.Transaction      ( CardNetwork(..)
                                                , MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionEvent(..)
                                                )
import           Shared.Models.User             ( UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.Utils
import           Shared.WebAPI.General.API      ( midToTrace )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldSatisfy
                                                )

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:15:32+00:00"

newtype NoGroup a =
    NoGroup { noGroup :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadMask
      , MonadThrow
      )

instance HasAccounts NoGroup where
  getGroupFor _ _ = return Nothing
instance HasTime NoGroup where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB NoGroup where
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
  saveLedgerTransaction _ _ _ = return ()
instance HasTransactionsDB NoGroup where
  getTransaction _ _ = return Nothing
  saveTransaction _ _ = return ()
instance HasRandom NoGroup where
  aRandomUUID = return nil

newtype PendingGroup a =
    PendingGroup { pendingGroup :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadMask
      , MonadThrow
      )

instance HasAccounts PendingGroup where
  getGroupFor _ _ = return . Just $ basicGroup { grpStatus = GroupPending }
  getUser _ = findUsers
instance HasTime PendingGroup where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB PendingGroup where
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
  saveLedgerTransaction _ _ _ = return ()
instance HasTransactionsDB PendingGroup where
  getTransaction _ _ = return Nothing
  saveTransaction _ _ = return ()
instance HasRandom PendingGroup where
  aRandomUUID = return nil

newtype JaneUnverified a =
    JaneUnverified { janeunverified :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadMask
      , MonadThrow
      )

instance HasAccounts JaneUnverified where
  getGroupFor _ _ = return . Just $ basicGroup { grpStatus = GroupPending }
  getUser _ u
    | u == usrUserID userJane = return . Just $ userJane
      { usrBankVerified = Just False
      }
    | otherwise = findUsers u
instance HasTime JaneUnverified where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB JaneUnverified where
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
  saveLedgerTransaction _ _ _ = return ()
instance HasTransactionsDB JaneUnverified where
  getTransaction _ _ = return Nothing
  saveTransaction _ _ = return ()
instance HasRandom JaneUnverified where
  aRandomUUID = return nil

newtype JanePending a =
    JanePending { janepending :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadThrow
      , MonadMask
      )

instance HasAccounts JanePending where
  getGroupFor _ _ = return . Just $ basicGroup { grpStatus = GroupPending }
  getUser _ u
    | u == usrUserID userJane = return . Just $ userJane
      { usrUserState = UserWaitingOnPII
      }
    | otherwise = findUsers u
instance HasTime JanePending where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB JanePending where
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
  saveLedgerTransaction _ _ _ = return ()
instance HasTransactionsDB JanePending where
  getTransaction _ _ = return Nothing
  saveTransaction _ _ = return ()
instance HasRandom JanePending where
  aRandomUUID = return nil

newtype HasPrevTrx a =
    HasPrevTrx { hasprevtrx :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadThrow
      , MonadMask
      )

instance HasAccounts HasPrevTrx where
  getGroupFor _ _ = return . Just $ basicGroup { grpStatus = GroupPending }
  getUser _ u
    | u == usrUserID userJane = return . Just $ userJane
      { usrUserState = UserWaitingOnPII
      }
    | otherwise = findUsers u
instance HasTime HasPrevTrx where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB HasPrevTrx where
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
  saveLedgerTransaction _ _ _ = return ()
instance HasTransactionsDB HasPrevTrx where
  getTransaction _ _ =
    return . Just $ basicTrx { trxGroupId = Just (grpId basicGroup, 1) }
  saveTransaction _ _ = return ()
instance HasRandom HasPrevTrx where
  aRandomUUID = return nil

spec :: Spec
spec = parallel $ do
  describe "Solo purchases" $ do
    it "Missing group" $ do
      trace <- midToTrace (MessageID nil)
      -- run test                       
      (prevTrxState, updatedTrx, ledgerEntrys) <- noGroup
        (updateTransaction (usrUserID userJohn) trace basicUpdates)

      null ledgerEntrys `shouldBe` True
      prevTrxState `shouldBe` TrxCreated
      -- is a solo trx
      trxGroupId updatedTrx `shouldBe` Nothing

    it "Group is pending group" $ do
      trace <- midToTrace (MessageID nil)
      -- run test                       
      (prevTrxState, updatedTrx, ledgerEntrys) <- pendingGroup
        (updateTransaction (usrUserID userJohn) trace basicUpdates)

      null ledgerEntrys `shouldBe` True
      prevTrxState `shouldBe` TrxCreated
      -- is a solo trx
      trxGroupId updatedTrx `shouldBe` Nothing

    it "Group is active but Jane is unverified" $ do
      trace <- midToTrace (MessageID nil)
      -- run test                       
      (prevTrxState, updatedTrx, ledgerEntrys) <- janeunverified
        (updateTransaction (usrUserID userJohn) trace basicUpdates)

      null ledgerEntrys `shouldBe` True
      prevTrxState `shouldBe` TrxCreated
      -- is a solo trx
      trxGroupId updatedTrx `shouldBe` Nothing

    it "Group is active but Jane is pending" $ do
      trace <- midToTrace (MessageID nil)
      -- run test                       
      (prevTrxState, updatedTrx, ledgerEntrys) <- janepending
        (updateTransaction (usrUserID userJohn) trace basicUpdates)

      null ledgerEntrys `shouldBe` True
      prevTrxState `shouldBe` TrxCreated
      -- is a solo trx
      trxGroupId updatedTrx `shouldBe` Nothing

    it "Don't change a group, even if it fails?" $ do
      -- run test                       
      trace <- midToTrace (MessageID nil)
      (prevTrxState, updatedTrx, ledgerEntrys) <- hasprevtrx
        (updateTransaction (usrUserID userJohn) trace basicUpdates)

      null ledgerEntrys `shouldBe` True
      prevTrxState `shouldBe` TrxCreated
      -- is a solo trx
      trxGroupId updatedTrx `shouldSatisfy` isJust

basicUpdates :: UpdatedTrxInfo
basicUpdates = UpdatedTrxInfo
  { utiTransaction      = trxId basicTrx
  , utiSource           = trxSource basicTrx
  , utiIdempotency      = Nothing
  , utiSourceId         = trxSourceId basicTrx
  , utiDetails          = CardTransaction { pcpContext         = "Testing"
                                          , pcpIsCardPresent   = True
                                          , pcpIsOnline        = True
                                          , pcpIsInternational = True
                                          , pcpNetwork         = Mastercard
                                          , pcpIsEMV           = True
                                          , pcpType            = Nothing
                                          , pcpDescription     = Nothing
                                          }
  , utiMerchant         = CardMerchant { cmiMcc      = MastercardMCC "0000"
                                       , cmiMccDesc  = ""
                                       , cmiName     = ""
                                       , cmiLocality = Nothing
                                       , cmiRegion   = Nothing
                                       , cmiCountry  = ""
                                       }
  , utiState            = TrxCompleted
  , utiCreatedAt        = unsafePerformIO Clock.getCurrentTime
  , utiTransactionEvent = AuthRequest
  , utiAmountLocal      = Currency "USD" 10
  , utiAmountHold       = Currency "USD" 10
  , utiAmountCashback   = Currency "USD" 0
  , utiAmountFee        = Currency "USD" 10
  , utiAmountBilling    = Currency "USD" 10
  , utiDescription      = Nothing
  , utiAdjustments      = []
  }

findUsers :: (Monad m) => UserID -> m (Maybe UserModel)
findUsers u | u == usrUserID userJohn = return $ Just userJohn
            | u == usrUserID userJane = return $ Just userJane
            | otherwise               = error "forgot to add this user"

