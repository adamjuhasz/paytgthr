{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module AFSM.Web.Referral.Referral where

import           AFSM.IO.Random                 ( HasRandom(..) )
import           AFSM.IO.Time                   ( GetCurrentTime(..) )
import           AFSM.Monad.HasEventTracking    ( (.=)
                                                , HasEventTracking(..)
                                                , object
                                                )
import           AFSM.Monad.HasGetUserDB        ( HasGetUserDB(getUserById) )
import           AFSM.Monad.HasPaymentAuthClient
                                                ( HasPaymentAuthClient(..) )
import           AFSM.Monad.HasReferralDB       ( HasReferralDB(..) )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromJust
                                                , listToMaybe
                                                )
import           Data.Ord                       ( Down(Down) )
import           Data.Time                      ( addUTCTime
                                                , nominalDay
                                                )
import           Data.Time.Clock                ( diffUTCTime )
import           Data.UUID                      ( fromText )
import           Servant                        ( NoContent(..)
                                                , ServerError(errBody)
                                                , err404
                                                , err409
                                                , err410
                                                )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Currency         ( Currency
                                                , currencyToDouble
                                                )
import           Shared.Models.Ids              ( JournalId(..)
                                                , ReferralProgramID
                                                , ReferralProgressID(..)
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(Manual) )
import           Shared.Models.Ledger.Journal   ( JournalSearch
                                                  ( GetPayTgthrRewards
                                                  )
                                                , LedgerJournal
                                                  ( LedgerJournal
                                                  , journalId
                                                  )
                                                )
import           Shared.Models.Referral.ReferralCode
                                                ( ReferralCode(..)
                                                , ReferralCodeDisplay
                                                )
import           Shared.Models.Referral.ReferralProgram
                                                ( ReferralPayout(..)
                                                , ReferralProgram(..)
                                                , ReferralWorkflow
                                                  ( Immediate
                                                  , PurchaseCount
                                                  , minCount
                                                  , qualifyingPurchaseMin
                                                  , timeLimitDays
                                                  )
                                                , RewardLevels(RewardLevels)
                                                )
import           Shared.Models.Referral.ReferralProgress
                                                ( ReferralProgress(..)
                                                , WorkFlowProgress(..)
                                                )
import           Shared.Models.User             ( UserModel
                                                  ( usrFirstName
                                                  , usrLastName
                                                  )
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

setReferralProgram
  :: (HasReferralDB m, MonadError ServerError m)
  => TraceContext
  -> ReferralProgram
  -> m ReferralProgram
setReferralProgram trace program = do
  saveReferralProgram trace program
  res <- getReferralProgram trace $ refProgram program
  case res of
    Nothing -> throwError err404
    Just rp -> return rp

getRererralCode
  :: (HasReferralDB m, MonadError ServerError m, GetCurrentTime m, HasRandom m)
  => TraceContext
  -> UserID
  -> m ReferralCode
getRererralCode trace uid = do
  res <- getReferralCodeForUID trace uid
  case res of
    Just rc -> return rc
    Nothing -> do
      public <- getPublicReferralProgram trace
      case public of
        Nothing ->
          throwError err410 { errBody = "No public progrsm to assign to" }
        Just ReferralProgram {..} -> do
          now  <- getCurrentTime
          code <- getRandomReferralCode
          let newCode = ReferralCode { referrerCode      = code
                                     , codeReferrerId    = Just uid
                                     , codeProgramLinked = refProgram
                                     , codeCreatedAt     = now
                                     }
          saveReferralCode trace newCode
          return newCode

useReferralCode
  :: (HasReferralDB m, MonadError ServerError m, HasRandom m, GetCurrentTime m)
  => TraceContext
  -> ReferralCodeDisplay
  -> UserID
  -> m NoContent
useReferralCode trace code uid = do
  ReferralCode {..} <- getReferralCode trace code >>= \case
    Nothing -> throwError err404 { errBody = "That code not found" }
    Just rc -> return rc

  ReferralProgram {..} <-
    getReferralProgram trace codeProgramLinked
      >>= (\case
            Nothing -> throwError err404 { errBody = "Program not found" }
            Just rp -> return rp
          )

  getReferralProgressFor trace uid >>= \case
    Nothing -> return ()
    Just _  -> throwError err409 { errBody = "Already have a progress" }


  newProgId <- getUUID
  now       <- getCurrentTime
  let newLink = ReferralProgress
        { progressId        = ReferralProgressID newProgId
        , referalProgram    = codeProgramLinked
        , referee           = uid
        , referrer          = codeReferrerId
        , programExpiration = case refWorkflow of
                                Immediate          -> Nothing
                                PurchaseCount {..} -> Just $ addUTCTime
                                  (nominalDay * fromIntegral timeLimitDays)
                                  now
        , progress          = case refWorkflow of
                                Immediate          -> ProgramCompleted
                                PurchaseCount {..} -> PurchaseCountProgress
                                  { refereeMade     = 0
                                  , programRequires = minCount
                                  }
        , progressDisplay   = 0
        , progressCreatedAt = now
        , progressUpdatedAt = now
        , progressRevision  = 1
        }

  saveReferralProgress trace newLink

  return NoContent

getReferralLinks
  :: (HasReferralDB m) => TraceContext -> UserID -> m [ReferralProgress]
getReferralLinks = getReferreeProgressFor

getReferralProgress
  :: (HasReferralDB m) => TraceContext -> UserID -> m (Maybe ReferralProgress)
getReferralProgress = getReferralProgressFor

setReferralProgress
  :: (HasReferralDB m) => TraceContext -> ReferralProgress -> m NoContent
setReferralProgress trace progrss = do
  saveReferralProgress trace progrss
  return NoContent

accessReferralProgram
  :: (HasReferralDB m, MonadError ServerError m)
  => TraceContext
  -> ReferralProgramID
  -> m ReferralProgram
accessReferralProgram trace pid = getReferralProgram trace pid >>= \case
  Nothing -> throwError err404
  Just rp -> return rp

updateReferralProgress
  :: ( HasReferralDB m
     , HasPaymentAuthClient m
     , MonadIO m
     , GetCurrentTime m
     , HasEventTracking m
     , HasGetUserDB m
     )
  => TraceContext
  -> UserID
  -> WorkFlowProgress
  -> m NoContent
updateReferralProgress trace uid prog = do
  currProgress <- getReferralProgress trace uid
  case currProgress of
    Nothing -> do
      traceError trace
                 "Error: Tried to update a nonexistant ReferralProgress"
                 (uid, prog)
      return NoContent
    Just ReferralProgress { progress = ProgramCompleted } -> return NoContent
    Just ReferralProgress { progress = ProgramExpired } -> return NoContent
    Just rp@ReferralProgress { progress = PurchaseCountProgress{}, ..} -> do
      now <- getCurrentTime
      let isExpiring = sum ((`diffUTCTime` now) <$> programExpiration)
      checkedProg <- if isExpiring >= 0
        then return prog
        else do
          tracePrint trace
                     "ReferralProgress expired"
                     (uid, programExpiration, now, prog, currProgress)
          return ProgramExpired
      let percent = case checkedProg of
            PurchaseCountProgress {..} ->
              ceiling
                $ toRational refereeMade
                / toRational programRequires
                * 100
            ProgramCompleted -> 100
            ProgramExpired   -> progressDisplay
      let newProgress = rp { progress          = checkedProg
                           , progressUpdatedAt = now
                           , progressRevision  = progressRevision + 1
                           , progressDisplay   = percent
                           }
      saveReferralProgress trace newProgress


      -- do we need to pay out?
      -- we know we start at PurchaseCountProgress
      case checkedProg of
        PurchaseCountProgress _ _ -> do
          userInfo <- getUserById uid
          let details = object
                [ "made" .= refereeMade checkedProg
                , "needs" .= programRequires checkedProg
                , "percent" .= percent
                , "firstName" .= (userInfo >>= usrFirstName)
                , "lastName" .= (userInfo >>= usrLastName)
                ]
          trackEventWithProps uid "User Referral PurchaseMade" details
          case referrer of
            Nothing -> return ()
            Just ui ->
              trackEventWithProps ui "Referee Referral PurchaseMade" details
          return ()
        ProgramExpired   -> return () -- shucks the program expired
        ProgramCompleted -> do
          ReferralProgram {..} <-
            getReferralProgram trace referalProgram >>= \case
              Nothing -> do
                let debug = (referalProgram, uid, prog, rp)
                traceError trace "Error: getReferralProgram failed " debug
                error $ "Error: getReferralProgram failed " <> show debug
              Just rp' -> return rp'

          -- for referrer
          case (referrer, referrerReward) of
            (Nothing, _) -> tracePrint trace "No referrer found" referrerReward
            (Just referrerUID, RewardLevels []) -> tracePrint
              trace
              "No payout levels found"
              (referrerUID, referrerReward)
            (Just referrerUID, RewardLevels payouts) -> do
              referees <- getReferreeProgressFor trace referrerUID
              let payoutEarned = referrReward referees payouts
              case payoutEarned of
                Nothing -> tracePrint trace
                                      "No qualifed levels found"
                                      (referrerUID, payouts)
                Just (CashPayout amount) -> do
                  tracePrint trace "Paying out referrerr" (referrerUID, amount)
                  userInfo <- getUserById uid
                  sendRewardToUser trace referrerUID amount
                  trackEventWithProps referrerUID "Referee Referral Earned"
                    $ object
                        [ "reward" .= case refereeReward of
                          CashPayout  cur -> Just $ currencyToDouble cur
                          BoostPayout _   -> Nothing
                        , "firstName" .= (userInfo >>= usrFirstName)
                        , "lastName" .= (userInfo >>= usrLastName)
                        ]
                  return ()
                Just (BoostPayout reward) -> do
                  traceError trace
                             "Error: tried to do BoostPayout"
                             (uid, reward)
                  error $ "Error: BoostPayout is not finished " <> show
                    (uid, reward)

          case refereeReward of
            CashPayout cur -> do
              sendRewardToUser trace referee cur
              trackEventWithProps uid "User Referral Earned"
                $ object ["reward" .= currencyToDouble cur]
            BoostPayout reward -> do
              traceError trace "Error: tried to do BoostPayout" (uid, reward)
              error $ "Error: BoostPayout is not finished " <> show
                (uid, reward)
          -- for referee


          return ()

      return NoContent

referralRewardsJournal :: JournalId
referralRewardsJournal =
  JournalId . fromJust . fromText $ "00000000-0000-0000-0000-000000000000" --Account Growth Rewards

sendRewardToUser
  :: (HasPaymentAuthClient m, MonadIO m)
  => TraceContext
  -> UserID
  -> Currency
  -> m ()
sendRewardToUser trace uid balance = do
  journals <- getLedger trace (GetPayTgthrRewards uid)
  case journals of
    [LedgerJournal { journalId }] ->
      journalTransfer trace referralRewardsJournal journalId $ Manual balance
    [] -> traceError trace "Error: user has no PayTgthrRewards ledger " uid
    x : y : z -> traceError
      trace
      "Error: user has too many PayTgthrRewards ledger "
      (uid, x, y, z)

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just $ last xs

referrReward :: [ReferralProgress] -> [(Int, b)] -> Maybe b
referrReward progresses payouts =
  let
    completed = filter (\rp' -> progress rp' == ProgramCompleted) progresses
    count     = length completed
    qualifiedLevels =
      sortOn (Down . fst) $ filter (\(i, _) -> count >= i) payouts
    payout = listToMaybe qualifiedLevels
  in
    snd <$> payout
