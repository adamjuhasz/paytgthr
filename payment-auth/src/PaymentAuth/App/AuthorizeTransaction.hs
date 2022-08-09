{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Redundant <&>" -}

module PaymentAuth.App.AuthorizeTransaction
  ( authorizeTransaction
  ) where

import           Control.Exception              ( SomeException )
import           Control.Monad                  ( forM )
import           Control.Monad.Catch            ( MonadCatch(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import           Data.Ord                       ( Down(Down) )
import           PaymentAuth.App.GetBalance     ( getSpendingLimit )
import           PaymentAuth.App.RiskManagement ( approveTransaction )
import           PaymentAuth.App.Utils          ( foldAdjustments )
import           PaymentAuth.Monad.Accounts     ( HasAccounts
                                                  ( getRewardsForGroup
                                                  )
                                                )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB )
import           Shared.Console                 ( traceError
                                                , tracePrint
                                                )
import           Shared.Models.Rewards.Boost    ( RewardBoost(..) )
import           Shared.Models.Rewards.Categorizer
                                                ( categorize )
import           Shared.Models.Transaction      ( Transaction(..) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult(..) )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

authorizeTransaction
  :: ( HasLedgerDB m
     , HasTransactionsDB m
     , HasPaymentsDB m
     , HasRiskScoresDB m
     , MonadIO m
     , HasAccounts m
     , MonadCatch m
     )
  => TraceContext
  -> Maybe Transaction
  -> Transaction
  -> m AuthResult
authorizeTransaction trace prevTrx trxCreated = do
  let incomingAmount = trxDisplayAmount trxCreated
  let idempotency    = trxSourceIdempotency trxCreated
  let hasPrevAuth =
        prevTrx
          <&> (\t -> t
                { trxDisplayAmount = incomingAmount + foldr foldAdjustments
                                                            0
                                                            (trxAdjustments t)
                , trxRevision      = trxRevision t + 1
                }
              )
  let normalizedAuth = fromMaybe trxCreated hasPrevAuth
  let debugId        = trxId normalizedAuth
  let usersInvolved  = fst <$> trxSplitAmounts trxCreated

  -- get all users' personal limits
  personalLimits <- zip usersInvolved
    <$> forM usersInvolved (getSpendingLimit trace)

  rewards <- case trxGroupId normalizedAuth of
    Nothing -> return []
    Just (gid, _) ->
      getRewardsForGroup trace gid
        `catch` (\(e :: SomeException) -> do
                  traceError trace "Error: could not get rewards " (debugId, e)
                  return []
                )

  let matchedRewards = sortOn (Down . boostRewardInBips)
        $ mapMaybe (categorize normalizedAuth) rewards
  let rewardToGive = case matchedRewards of
        []                   -> Nothing
        RewardBoost {..} : _ -> Just boostId

  -- debug info
  tracePrint trace
             "(trxId, personalLimits, trxDisplayAmount): "
             (debugId, personalLimits, trxDisplayAmount normalizedAuth)

  let trxNormalized = normalizedAuth { trxSourceIdempotency = idempotency
                                     , trxMsgSource         = traceToMID trace
                                     , trxRewardId          = rewardToGive
                                     }
  -- the meat
  purchaserRiskScore <- getRiskScoreOf trace $ trxUserId trxCreated
  let (_, authRes) =
        approveTransaction purchaserRiskScore trxNormalized personalLimits

  return authRes
