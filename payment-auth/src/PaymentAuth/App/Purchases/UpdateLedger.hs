{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module PaymentAuth.App.Purchases.UpdateLedger
  ( updateLedgerAsNeeded
  ) where

import           Control.Exception              ( SomeException
                                                , throw
                                                )
import           Control.Monad.Catch            ( MonadCatch(..)
                                                , MonadMask
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                , mapMaybe
                                                )
import           Data.Ratio                     ( (%) )
import           Data.UUID                      ( fromText )
import           PaymentAuth.App.Ledger.CreateFacts
                                                ( createAdjFacts )
import           PaymentAuth.App.Ledger.Update  ( FactTuple
                                                , createLedgerTransaction
                                                )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency(Currency)
                                                , getMonetaryValue
                                                , roundUpUSD
                                                )
import           Shared.Models.Ids              ( JournalId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..)
                                                , factAmount
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalType(..)
                                                , LedgerJournal(journalId)
                                                , journalOwner
                                                , searchFor
                                                )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction(ltxEntries)
                                                , TransactionEntries
                                                  ( Entries
                                                  , EntriesById
                                                  )
                                                )
import           Shared.Models.Rewards.Boost    ( RewardBoost(boostRewardInBips)
                                                )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , AptoAdjustmentID(..)
                                                , AptoAdjustmentType(..)
                                                , Transaction(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

-- common error handler
handler
  :: (MonadIO m, Show a)
  => a
  -> SomeException
  -> m (LedgerEntry, LedgerTransaction)
handler debug e = do
  let errorDebug = (e, debug)
  liftIO $ putStr "Error: updateLedgerAsNeeded " >> print errorDebug
  throw e

getExistingSavedAdjustmets
  :: (HasLedgerDB m)
  => TraceContext
  -> JournalId
  -> AptoAdjustment
  -> m (AptoAdjustment, Maybe LedgerEntry)
getExistingSavedAdjustmets trace jid adj@AptoAdjustment { adjId = (AptoAdjustmentID idem) }
  = (adj, ) <$> getLedgerForIdem trace jid idem

patriotSettlmentJournal :: JournalId
patriotSettlmentJournal =
  case fromText "00000000-0000-0000-0000-000000000000" of
    Nothing -> error "not a uuid"
    Just u  -> JournalId u

rewardsJournal :: JournalId
rewardsJournal = case fromText "00000000-0000-0000-0000-000000000000" of
  Nothing -> error "not a uuid"
  Just u  -> JournalId u

type FromJournal = JournalId
type ToJournal = JournalId
updateLedger
  :: ( HasAccounts m
     , HasLedgerDB m
     , HasTime m
     , HasRandom m
     , MonadIO m
     , MonadMask m
     )
  => TraceContext
  -> FromJournal
  -> ToJournal
  -> FactTuple
  -> m (LedgerEntry, LedgerTransaction)
updateLedger trace from to (aUser, idem, fact) = do
  ledgerTransaction <- createLedgerTransaction trace idem from to fact

  let trxDebug = show (ledgerTransaction, aUser, trace)
  let entries = case ltxEntries ledgerTransaction of
        EntriesById _   -> error $ "Error: need full entries " <> trxDebug
        Entries     les -> les

  let isUserEntry  = (==) to . lenJournal

  let entriesDebug = show (entries, aUser, trace)
  let userEntry = case filter isUserEntry entries of
        [le]  -> le
        []    -> error $ "Error: no entries found for " <> entriesDebug
        _ : _ -> error $ "Error: multiple entries found for " <> entriesDebug

  return (userEntry, ledgerTransaction)

updateLedgerAsNeeded
  :: ( HasAccounts m
     , HasLedgerDB m
     , HasTime m
     , HasRandom m
     , MonadIO m
     , MonadMask m
     )
  => TraceContext
  -> Transaction
  -> JournalType
  -> m [(LedgerEntry, LedgerTransaction)]
updateLedgerAsNeeded trace updatedTrx journal = do
  let mid   = traceToMID trace
  let tid   = trxId updatedTrx
  let owner = UserID <$> journalOwner journal
  let defaultFiler x = case adjType x of
        CaptureAdjustment       -> True
        RefundAdjustment        -> True
        AuthorizationAdjustment -> False
  let filterer x = case journal of
        PayTgthrRewards _ -> True
        PayTgthr        _ -> defaultFiler x
        StashTgthr      _ -> defaultFiler x
        SaveTgthr       _ -> defaultFiler x
        SecurtyDeposit  _ -> defaultFiler x
        FundingSource _ _ -> defaultFiler x
        ExternalAccount _ -> defaultFiler x
        VirtualAccount    -> defaultFiler x
  let finalAdjustments = filter filterer $ trxAdjustments updatedTrx

  -- find existing entries using the adjustment id as the idemptotency key
  journalList <- getLedgerJournalType trace $ searchFor journal
  journalid   <- case journalList of
    [] -> do
      traceError trace "Error: No journal of type " (owner, journal)
      error $ "Error: No journal of type " <> show (owner, journal)
    [     lj] -> return $ journalId lj
    _ : _ : _ -> do
      let debugInfo = (owner, journal, journalList)
      traceError trace "Error: too many journals of type " debugInfo
      error $ "Error: too many journals of type " <> show debugInfo

  existingAdjustments <- mapM (getExistingSavedAdjustmets trace journalid)
                              finalAdjustments
  let existingEntries = mapMaybe snd existingAdjustments

  -- filter out adjustments already saved to user's ledger
  let newAdjustments =
        fst <$> filter (\(_, m) -> isNothing m) existingAdjustments

   -- this creates for each user
  let trxInfo      = (trxId updatedTrx, trxSplitAmounts updatedTrx)
  let newEntries   = createAdjFacts trxInfo newAdjustments

  let usersEntries = filter (\(u, _, _) -> Just u == owner) newEntries

  -- each journal type has its own update function
  case journal of
    PayTgthr _ -> do
      let ledgerUpdater x =
            updateLedger trace patriotSettlmentJournal journalid x
              `catch` handler (journal, tid)

      tracePrint trace
                 "updateLedgerAsNeeded PayTgthr "
                 (journal, tid, usersEntries, mid, trxSplitAmounts updatedTrx)

      mapM ledgerUpdater usersEntries

    PayTgthrRewards _ -> do
      let sumOfExistingRewards =
            sum $ fmap (factAmount . lenFact) existingEntries
      let sumOfCurrentEntries =
            sum $ fmap (\(_, _, f) -> factAmount f) usersEntries
      reward <- case trxRewardId updatedTrx of
        Nothing -> return 100
        Just ri -> boostRewardInBips <$> getReward trace ri
      let rewardAsCurrency = Currency "USD" $ reward % 10000
      let calcReward :: Currency -> Currency
          calcReward c = roundUpUSD $ (-1) * c * rewardAsCurrency
      let expectedRewards = calcReward sumOfCurrentEntries
      let rewardsToGive   = expectedRewards - sumOfExistingRewards
      let existingIdems   = fmap lenIdempotency existingEntries
      let currentIdems    = fmap (\(_, i, _) -> i) usersEntries
      let idem = case newAdjustments of
            [] -> Nothing
            AptoAdjustment { adjId = AptoAdjustmentID i } : _ -> Just i

      -- debug
      tracePrint
        trace
        "PayTgthrRewards 1 "
        ( owner
        , ("rewardsToGive" :: String       , rewardsToGive)
        , ("expectedRewards" :: String     , expectedRewards)
        , ("sumOfExistingRewards" :: String, sumOfExistingRewards)
        , usersEntries
        , ("existingIdems" :: String, existingIdems)
        , ("currentIdems" :: String , currentIdems)
        , ("idem" :: String         , idem)
        )
      tracePrint trace
                 "PayTgthrRewards 2 "
                 (owner, existingEntries, usersEntries)

      let actualowner =
            fromMaybe (error $ "Must have an owner " <> show journal) owner

      res <- if getMonetaryValue rewardsToGive /= 0
        then (: []) <$> updateLedger
          trace
          rewardsJournal
          journalid
          (actualowner, idem, TrxAdjustment tid rewardsToGive)
        else return []

      tracePrint trace "PayTgthrRewards 3 " (owner, res)

      return res
    StashTgthr     _  -> error "Error: StashTgthr and uLedger not work"
    SaveTgthr      _  -> error "Error: SaveTgthr and uLedger not work"
    SecurtyDeposit _  -> error "Error: SecurtyDeposit and uLedger not work"
    FundingSource _ _ -> error "Error: FundingSource and uLedger not work"
    ExternalAccount _ -> error "Error: ExternalAccount and uLedger not work"
    VirtualAccount    -> error "Error: VirtualAccount and uLedger not work"
