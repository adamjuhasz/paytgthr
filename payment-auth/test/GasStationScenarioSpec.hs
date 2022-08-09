{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use head" -}

{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module GasStationScenarioSpec
  ( spec
  ) where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO(liftIO) )
import           Data.Aeson                    as A
                                                ( decode
                                                , eitherDecode
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.List                      ( nub
                                                , sort
                                                )
import           Data.Maybe                     ( fromJust )
import           Data.Ratio                     ( (%)
                                                , Ratio
                                                )
import           Data.Time.Clock                ( UTCTime )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           Data.UUID.V4                   ( nextRandom )
import           PaymentAuth.App.Ledger.Update  ( patriotSettlmentJournal )
import           PaymentAuth.App.Purchases.CreateTransaction
                                                ( createTransaction )
import qualified PaymentAuth.App.Purchases.UpdateTransaction
                                               as CB
import           PaymentAuth.App.Purchases.UpdateTrx
                                                ( UpdatedTrxInfo(..)
                                                , updateTransaction
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Groups             ( basicGroup )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.Apto.Transaction ( AptoTransaction(..)
                                                , AptoTransactionId(..)
                                                , TransactionDetails(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Group            ( GroupModel(..) )
import           Shared.Models.Ids              ( JournalId(..)
                                                , LedgerEntryId(..)
                                                , LedgerTrxId(LedgerTrxId)
                                                , MessageID(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(..)
                                                , LedgerJournal(..)
                                                )
import           Shared.Models.Transaction     as T
                                                ( Transaction(..)
                                                , TransactionEvent(AuthRequest)
                                                , TransactionId(..)
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API      ( midToTrace )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )

aUserID :: UserID
{-# NOINLINE aUserID #-}
aUserID = unsafePerformIO (UserID <$> nextRandom)

aMessageId :: MessageID
{-# NOINLINE aMessageId #-}
aMessageId = unsafePerformIO (MessageID <$> nextRandom)

aTransactionId :: TransactionId
{-# NOINLINE aTransactionId #-}
aTransactionId = unsafePerformIO (TransactionId <$> nextRandom)

almostNow :: UTCTime
{-# NOINLINE almostNow #-}
almostNow = unsafePerformIO Clock.getCurrentTime

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:15:32+00:00"

ninenineCents :: Ratio Integer
ninenineCents = (-4458563631096791) % 4503599627370496

aptTrxToUpdateTrx :: AptoTransaction -> UpdatedTrxInfo
aptTrxToUpdateTrx AptoTransaction { aptSourceId = AptoTransactionId srcID, ..}
  = let utcTransaction      = aTransactionId
        utcSource           = aptSource
        utcIdempotency      = aptIdempotency
        utcSourceId         = srcID
        utcDetails          = aptDetails
        utcMerchant         = aptMerchant
        utcState            = aptState
        utcCreatedAt        = aptCreatedAt
        utcTransactionEvent = aptTransactionEvent
        utcAmountLocal      = aptAmountLocal
        utcAmountHold       = aptAmountHold
        utcAmountCashback   = aptAmountCashback
        utcAmountFee        = aptAmountFee
        utcAmountBilling    = aptAmountBilling
        utcDescription      = pcpDescription aptDetails
        utcAdjustments      = aptAdjustments
    in  UpdatedTrxInfo { utiTransaction      = utcTransaction
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
aptTrxToUpdateTrx _ = error "Incomplete mock"

gasStationStep1 :: Transaction
gasStationStep1 = createTransaction aMessageId
                                    aTransactionId
                                    TrxAuthorized
                                    AuthRequest
                                    (Currency "USD" ninenineCents)
                                    aUserID
                                    Nothing
                                    Apto
                                    "1"
                                    Nothing
                                    Nothing
                                    (Just "Targé")
                                    almostNow

gasStationStep2 :: UpdatedTrxInfo
{-# NOINLINE gasStationStep2 #-}
gasStationStep2 = aptTrxToUpdateTrx $ unsafePerformIO
  (fromJust . A.decode <$> BSL.readFile
    "test/json/scenario_gas_station/02-pending.json"
  )

gasStationStep3 :: UpdatedTrxInfo
{-# NOINLINE gasStationStep3 #-}
gasStationStep3 = aptTrxToUpdateTrx $ unsafePerformIO
  (fromJust . A.decode <$> BSL.readFile
    "test/json/scenario_gas_station/03-complete-reversal.json"
  )

errorRight :: Show a => Either a p -> p
errorRight (Right x) = x
errorRight (Left  e) = error (show e)

gasStationStep5 :: UpdatedTrxInfo
{-# NOINLINE gasStationStep5 #-}
gasStationStep5 = aptTrxToUpdateTrx $ unsafePerformIO
  (errorRight . A.eitherDecode <$> BSL.readFile
    "test/json/scenario_gas_station/05-complete-pending.json"
  )

gasStationStep6 :: UpdatedTrxInfo
{-# NOINLINE gasStationStep6 #-}
gasStationStep6 = aptTrxToUpdateTrx $ unsafePerformIO
  (fromJust . A.decode <$> BSL.readFile
    "test/json/scenario_gas_station/06-complete-purchase.json"
  )

splits :: [(UserID, Rational)]
splits = [(usrUserID userJohn, 50), (usrUserID userJane, 50)]

authedTrx :: Transaction
authedTrx = gasStationStep1
  { trxSplitAmounts = splits
  , trxGroupId      = Just (grpId basicGroup, grpRevision basicGroup)
  }

pendingTrx :: Transaction
pendingTrx = updateTransaction aMessageId
                               gasStationStep2
                               almostNow
                               aUserID
                               (Just basicGroup)
                               (Just authedTrx)

completeRev :: Transaction
completeRev = updateTransaction aMessageId
                                gasStationStep3
                                almostNow
                                aUserID
                                (Just basicGroup)
                                (Just pendingTrx)

completePen :: Transaction
completePen = updateTransaction aMessageId
                                gasStationStep5
                                almostNow
                                aUserID
                                Nothing
                                (Just completeRev)

completePur :: Transaction
completePur = updateTransaction aMessageId
                                gasStationStep6
                                almostNow
                                aUserID
                                Nothing
                                (Just completePen)

patriotEntryId :: LedgerEntryId
{-# NOINLINE patriotEntryId #-}
patriotEntryId = unsafePerformIO (LedgerEntryId <$> nextRandom)

patriotJournal :: LedgerJournal
patriotJournal = LedgerJournal { journalId             = patriotSettlmentJournal
                               , journalType           = VirtualAccount
                               , journalName           = ""
                               , journalUser           = Nothing
                               , lastJournalEntry      = patriotEntryId
                               , journalBalance        = Currency "USD" 0
                               , journalPendingBalance = Currency "USD" 0
                               , journalRevision       = 1
                               , journalCreated        = aTime
                               , journalUpdated        = aTime
                               , journalTransaction    = LedgerTrxId nil
                               }

patriotEntry :: LedgerEntry
patriotEntry = LedgerEntry { lenId             = patriotEntryId
                           , lenJournal        = patriotSettlmentJournal
                           , lenUser           = Nothing
                           , lenRevision       = 1
                           , lenVersion        = "1.0"
                           , lenMsgSource      = MessageID nil
                           , lenBalance        = Currency "USD" 0
                           , lenPendingBalance = Currency "USD" 0
                           , lenFact           = Manual $ Currency "USD" 0
                           , lenIdempotency    = Nothing
                           , lenCreatedAt      = aTime
                           , lenTransaction    = Nothing
                           }

janeEntryId :: LedgerEntryId
{-# NOINLINE janeEntryId #-}
janeEntryId = unsafePerformIO (LedgerEntryId <$> nextRandom)

janeJournalId :: JournalId
{-# NOINLINE janeJournalId #-}
janeJournalId = unsafePerformIO (JournalId <$> nextRandom)

janeJournal :: LedgerJournal
janeJournal = patriotJournal { journalId        = janeJournalId
                             , lastJournalEntry = janeEntryId
                             , journalUser      = Just $ usrUserID userJane
                             }

janeEntry :: LedgerEntry
janeEntry = patriotEntry { lenId      = janeEntryId
                         , lenJournal = janeJournalId
                         , lenUser    = Just $ usrUserID userJane
                         }

johnEntryId :: LedgerEntryId
{-# NOINLINE johnEntryId #-}
johnEntryId = unsafePerformIO (LedgerEntryId <$> nextRandom)

johnJournalId :: JournalId
{-# NOINLINE johnJournalId #-}
johnJournalId = unsafePerformIO (JournalId <$> nextRandom)

johnJournal :: LedgerJournal
johnJournal = patriotJournal { journalId        = johnJournalId
                             , lastJournalEntry = johnEntryId
                             , journalUser      = Just $ usrUserID userJohn
                             }

johnEntry :: LedgerEntry
johnEntry = patriotEntry { lenId      = johnEntryId
                         , lenJournal = johnJournalId
                         , lenUser    = Just $ usrUserID userJohn
                         }

journalDB :: (Monad m) => JournalId -> m (Maybe LedgerJournal)
journalDB jid | jid == patriotSettlmentJournal = return $ Just patriotJournal
              | jid == janeJournalId           = return $ Just janeJournal
              | jid == johnJournalId           = return $ Just johnJournal
              | otherwise = error $ "journalDB " <> show jid

entryDB :: Monad m => LedgerEntryId -> m (Maybe LedgerEntry)
entryDB leid | leid == patriotEntryId = return $ Just patriotEntry
             | leid == janeEntryId    = return $ Just janeEntry
             | leid == johnEntryId    = return $ Just johnEntry
             | otherwise              = error $ "entryDB " <> show leid

journalSearch :: (Monad m) => JournalSearch -> m [LedgerJournal]
journalSearch (GetPayTgthr u)
  | u == usrUserID userJohn = return [johnJournal]
  | u == usrUserID userJane = return [janeJournal]
  | otherwise = error $ "journalSearch " <> show
    (u, usrUserID userJane, usrUserID userJohn)
journalSearch (GetPayTgthrRewards _) = return []
journalSearch e                      = error $ "no search " <> show e

newtype Step1Monad a =
    Step1Monad { unStep1Monad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadThrow
      , MonadMask
      )

instance HasAccounts Step1Monad where
  getGroupFor _ _ = return $ Just basicGroup
  getUser _ u | u == usrUserID userJohn = return $ Just userJohn
              | u == usrUserID userJane = return $ Just userJane
              | otherwise               = error "forgot to add this user"

instance HasTime Step1Monad where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB Step1Monad where
  getLedgerJournal _ = journalDB
  getLedgerEntry _ = entryDB
  saveLedgerTransaction _ _ _ = return ()
  getLedgerJournalType _ = journalSearch
  getLedgerForIdem _ _ _ = return Nothing
instance HasTransactionsDB Step1Monad where
  getTransaction _ _ = return Nothing
  saveTransaction _ _ = return ()
instance HasRandom Step1Monad where
  aRandomUUID = return nil

newtype Step2Monad a =
    Step2Monad { unStep2Monad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadThrow
      , MonadMask
      )

instance HasAccounts Step2Monad where
  getGroupFor _ _ = return $ Just basicGroup
  getUser _ u | u == usrUserID userJohn = return $ Just userJohn
              | u == usrUserID userJane = return $ Just userJane
              | otherwise               = error "forgot to add this user"
instance HasTime Step2Monad where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB Step2Monad where
  getLedgerForIdem _ _ _ = return Nothing
  getLedgerJournal _ = journalDB
  getLedgerEntry _ = entryDB
  saveLedgerTransaction _ _ _ = return ()
  getLedgerJournalType _ = journalSearch
instance HasTransactionsDB Step2Monad where
  getTransaction _ _ = return $ Just pendingTrx
  saveTransaction _ _ = return ()
instance HasRandom Step2Monad where
  aRandomUUID = return nil

newtype Step3Monad a =
    Step3Monad { unStep3Monad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadCatch
      , MonadThrow
      , MonadMask
      )

instance HasAccounts Step3Monad where
  getGroupFor _ _ = return $ Just basicGroup
  getUser _ u | u == usrUserID userJohn = return $ Just userJohn
              | u == usrUserID userJane = return $ Just userJane
              | otherwise               = error "forgot to add this user"
instance HasTime Step3Monad where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB Step3Monad where
  getLedgerForIdem _ _ _ = return Nothing
  getLedgerJournal _ = journalDB
  getLedgerEntry _ = entryDB
  saveLedgerTransaction _ _ _ = return ()
  getLedgerJournalType _ = journalSearch
instance HasTransactionsDB Step3Monad where
  getTransaction _ _ = return $ Just completePen
  saveTransaction _ _ = return ()
instance HasRandom Step3Monad where
  aRandomUUID = return nil

spec :: Spec
spec = do
  describe "Gas station lifecycle" $ do

    it "authed -> pending" $ do
      trace <- midToTrace aMessageId
      T.trxState pendingTrx `shouldBe` TrxPending

      (_, _, ledgerEntrys) <- unStep1Monad
        (CB.updateTransaction aUserID trace gasStationStep2)
      null ledgerEntrys `shouldBe` True

    it "pending -> complete (reversal)" $ do
      trace <- midToTrace aMessageId
      T.trxState completeRev `shouldBe` TrxCompleted
      T.trxSplitAmounts completeRev `shouldBe` splits

      (_, _, ledgerEntrys) <- unStep2Monad
        (CB.updateTransaction aUserID trace gasStationStep3)
      length ledgerEntrys `shouldBe` 4 -- 2 for each partner (2 * 2)
      let getVal adj = case adj of
            TrxAdjustment _ val -> val
            _                   -> error "Not TrxAdjustment"

      let getIdem i = lenIdempotency (ledgerEntrys !! i)

      getIdem 0 `shouldBe` Just "adj_01e6e418e744191e"
      getIdem 2 `shouldBe` Just "adj_01e6e418e744191e"
      getIdem 1 `shouldBe` Just "adj_24e0a1aba6718d64"
      getIdem 3 `shouldBe` Just "adj_24e0a1aba6718d64"

      let ledgerUser i = lenUser (ledgerEntrys !! i)

      ledgerUser 0 `shouldBe` Just (usrUserID userJohn)
      ledgerUser 2 `shouldBe` Just (usrUserID userJane)
      ledgerUser 1 `shouldBe` Just (usrUserID userJohn)
      ledgerUser 3 `shouldBe` Just (usrUserID userJane)

      let factVal i = getVal $ lenFact $ ledgerEntrys !! i

      sum [factVal 0, factVal 1, factVal 2, factVal 3]
        `shouldBe` Currency "USD" 0
      factVal 0 `shouldBe` Currency "USD" ((-38) % 1)
      factVal 2 `shouldBe` Currency "USD" ((-38) % 1)
      factVal 1 `shouldBe` Currency "USD" (38 % 1)
      factVal 3 `shouldBe` Currency "USD" (38 % 1)

    it "complete (reversal) -> complete (pending)" $ do
      T.trxState completePen `shouldBe` TrxCompleted

    it "complete (pending) -> complete (purchase)" $ do
      T.trxState completePur `shouldBe` TrxCompleted

      trace                <- midToTrace aMessageId
      (_, _, ledgerEntrys) <- unStep3Monad
        (CB.updateTransaction aUserID trace gasStationStep6)
      length ledgerEntrys `shouldBe` 6
      length (sort . nub $ fmap lenIdempotency ledgerEntrys) `shouldBe` 3
      (sort . nub $ fmap lenIdempotency ledgerEntrys)
        `shouldBe` [ Just "adj_01e6e418e744191e"
                   , Just "adj_24e0a1aba6718d64"
                   , Just "adj_c2bc0f4782e28fd9"
                   ]
