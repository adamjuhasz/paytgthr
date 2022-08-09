{-# LANGUAGE RecordWildCards #-}
module LandingPage.Handlers.Application.Rewards where

import           Control.Monad                  ( forM )
import           Data.Aeson                     ( KeyValue((.=))
                                                , object
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromJust
                                                , mapMaybe
                                                )
import           Data.UUID                      ( fromText )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Vault.Lazy               as V
import           LandingPage.Types              ( SessionData )
import           LandingPage.Utils              ( createTrace
                                                , expectSession
                                                , requireUser
                                                )
import           Network.HTTP.Types             ( status500 )
import           Network.Wai                    ( Request(vault) )
import           Servant.Client                 ( ClientEnv
                                                , runClientM
                                                )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Group            ( GroupModel(grpId, grpStatus)
                                                , GroupStatus(GroupActive)
                                                )
import           Shared.Models.Ids              ( ActivatedRewardId(..)
                                                , JournalId(..)
                                                , RewardId(RewardId)
                                                , TransactionId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(..)
                                                , LedgerJournal(..)
                                                )
import           Shared.Models.Rewards.Boost    ( RewardBoost )
import           Shared.Models.Transaction      ( Transaction(..) )
import           Shared.WebAPI.AccountsFSM.API  ( ActivateRewardBody(..)
                                                , Routes(..)
                                                )
import           Shared.WebAPI.AccountsFSM.Client
                                                ( accountsClientM )
import           Shared.WebAPI.PaymentAuth.API  ( CreateLedgerJournalBody(..)
                                                , CreateLedgerTrxBody(..)
                                                , Routes(..)
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( payAuthClientM )
import           Web.Scotty                    as Scotty
                                                ( ActionM
                                                , finish
                                                , json
                                                , liftAndCatchIO
                                                , param
                                                , request
                                                , status
                                                )

getGroup :: Shared.Models.Ids.UserID -> ClientEnv -> ActionM [GroupModel]
getGroup user accountsEnv = do
  trace <- createTrace
  let getGroupFn = _GroupsForUser accountsClientM trace user [GroupActive]

  groupModelE <- liftAndCatchIO $ runClientM getGroupFn accountsEnv

  let groups = case groupModelE of
        Left  e -> error $ show (user, e)
        Right g -> sortOn grpStatus g
  return groups

getActiveRewards :: ClientEnv -> ActionM ()
getActiveRewards accountsEnv = do
  trace <- createTrace
  let getRewards = _RewardsGetList accountsClientM trace
  rewardsE <- liftAndCatchIO $ runClientM getRewards accountsEnv

  case rewardsE of
    Left  e -> error $ show e
    Right r -> Scotty.json r

getOurRewards :: V.Key SessionData -> ClientEnv -> ActionM ()
getOurRewards sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace   <- createTrace

  groups  <- getGroup user accountsEnv

  groupid <- case groups of
    []    -> Scotty.json ([] :: [RewardBoost]) >> finish
    g : _ -> return $ grpId g

  let getRewards = _GroupGetRewards accountsClientM trace groupid
  rewardsE <- liftAndCatchIO $ runClientM getRewards accountsEnv

  case rewardsE of
    Left  e -> error $ show (user, e)
    Right r -> Scotty.json r

activateReward :: V.Key SessionData -> ClientEnv -> ActionM ()
activateReward sKey accountsEnv = do
  user <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace   <- createTrace
  rid     <- RewardId . fromJust . fromText <$> param "rewardid"

  groups  <- getGroup user accountsEnv

  groupid <- case groups of
    []    -> Scotty.json ([] :: [RewardBoost]) >> finish
    g : _ -> return $ grpId g

  newId <- ActivatedRewardId <$> liftAndCatchIO nextRandom
  let getRewards =
        _GroupActivateReward accountsClientM trace groupid $ ActivateRewardBody
          { activatedBy      = user
          , rewardToActivate = rid
          , newActivationId  = newId
          }
  rewardsE <- liftAndCatchIO $ runClientM getRewards accountsEnv

  case rewardsE of
    Left  e -> error $ show (user, e)
    Right r -> Scotty.json r

getRewardEntries :: V.Key SessionData -> ClientEnv -> ActionM ()
getRewardEntries sKey payAuthEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  let errorHandelr e = do
        liftAndCatchIO $ putStr "Error: Could not getRewardEntries " >> print
          (user, e)
        Scotty.status status500
          >> Scotty.json (object ["error" .= True])
          >> finish

  let getJournal = _GetJournal payAuthClientM trace (GetPayTgthrRewards user)
  journalE <- liftAndCatchIO $ runClientM getJournal payAuthEnv
  journal  <- case journalE of
    Left  e  -> errorHandelr e
    Right [] -> do
      jid <- JournalId <$> liftAndCatchIO nextRandom
      let createJournal = _CreateJournal
            payAuthClientM
            trace
            CreateLedgerJournalBody { newJournalId   = jid
                                    , newJournalType = PayTgthrRewards user
                                    , newJournalName = "PayTgthrRewards"
                                    , startBalance   = Currency "USD" 0
                                    }
      newJournalE <- liftAndCatchIO $ runClientM createJournal payAuthEnv
      case newJournalE of
        Left  e -> errorHandelr e
        Right _ -> return ()
      journalTry2E <- liftAndCatchIO $ runClientM getJournal payAuthEnv
      case journalTry2E of
        Left  e  -> errorHandelr e
        Right [] -> do
          liftAndCatchIO
            $  putStr "Error: Could not create PayTgthrRewards "
            >> print user
          Scotty.status status500
            >> Scotty.json (object ["error" .= True])
            >> finish
        Right (j : _) -> return j
    Right (j : _) -> return j

  let getEntries = _GetEntries payAuthClientM trace $ journalId journal
  entriesE <- liftAndCatchIO $ runClientM getEntries payAuthEnv
  entries  <- case entriesE of
    Left  e -> errorHandelr e
    Right r -> return r

  let isTrxFact :: LedgerEntry -> Maybe (Integer, TransactionId, Currency)
      isTrxFact LedgerEntry { lenFact = TrxAdjustment tid amt, ..} =
        Just (lenRevision, tid, amt)
      isTrxFact _ = Nothing
  let keyedEntries = mapMaybe isTrxFact entries

  keyeedTransaction <- forM
    keyedEntries
    (\(rev, tid, amt) -> do
      let getPurchase = _GetPurchase payAuthClientM trace tid
      trxE <- liftAndCatchIO $ runClientM getPurchase payAuthEnv
      trx  <- case trxE of
        Left  e -> error $ show (user, e)
        Right t -> return t
      return (rev, tid, amt, trx)
    )

  Scotty.json $ object
    [ "balance" .= journalBalance journal
    , "entries"
      .= (keyeedTransaction <&> \(rev, tid, amt, trx) -> object
           [ "revision" .= rev
           , "transaction" .= tid
           , "amount" .= amt
           , "description" .= trxDescription trx
           , "reward" .= trxRewardId trx
           ]
         )
    ]

transferRewardBalance :: V.Key SessionData -> ClientEnv -> ActionM ()
transferRewardBalance sKey payAuthEnv = do
  user  <- request <&> vault <&> V.lookup sKey <&> expectSession >>= requireUser
  trace <- createTrace

  let errorHandelr e = do
        liftAndCatchIO
          $  putStr "Error: Could not transferRewardBalance "
          >> print (user, e)
        Scotty.status status500
          >> Scotty.json (object ["error" .= True])
          >> finish

  let getRewardsJournal =
        _GetJournal payAuthClientM trace (GetPayTgthrRewards user)
  journalRewardsE <- liftAndCatchIO $ runClientM getRewardsJournal payAuthEnv
  journalRewards  <- case journalRewardsE of
    Left e -> errorHandelr e
    Right [] ->
      Scotty.status status500
        >> Scotty.json (object ["error" .= True])
        >> finish
    Right (j : _) -> return j

  let getPTJournal = _GetJournal payAuthClientM trace (GetPayTgthr user)
  journalPTE <- liftAndCatchIO $ runClientM getPTJournal payAuthEnv
  journalPT  <- case journalPTE of
    Left e -> errorHandelr e
    Right [] ->
      Scotty.status status500
        >> Scotty.json (object ["error" .= True])
        >> finish
    Right (j : _) -> return j

  let transfer = _CreateJournalTrx
        payAuthClientM
        trace
        CreateLedgerTrxBody
          { fromJournal = journalId journalRewards
          , toJournal   = journalId journalPT
          , fact        = UserTransfer $ journalBalance journalRewards
          }
  journalTrxE <- liftAndCatchIO $ runClientM transfer payAuthEnv
  case journalTrxE of
    Left  e -> errorHandelr e
    Right _ -> Scotty.json (object ["success" .= True])
