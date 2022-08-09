{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Reports.Generate where

import           Chewpaca.DB.Groups             ( getJustGroupByIdAndRev )
import           Chewpaca.DB.Ledger             ( getNewRewardJournals
                                                , getNewRewardJournalsForUser
                                                )
import           Chewpaca.DB.PaymentAuth        ( getGlobalPurchasesInLast
                                                , getUserPurchasesInLast
                                                )
import           Chewpaca.DB.Rewards            ( getAllRewards )
import           Chewpaca.DB.RiskScores         ( getCurrentRiskForUser )
import           Chewpaca.DB.Users              ( getActiveUsers
                                                , getJustUser
                                                )
import           Chewpaca.Utils.Aggregation     ( groupedByGID
                                                , mapSnd
                                                , mean
                                                , median
                                                , sortAndGroupBy
                                                )
import           Control.Monad                  ( forM
                                                , forM_
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Aeson                     ( KeyValue((.=))
                                                , Value
                                                , object
                                                )
import           Data.Bifunctor                 ( Bifunctor
                                                  ( bimap
                                                  , first
                                                  , second
                                                  )
                                                )
import           Data.List                      ( find
                                                , groupBy
                                                , sortOn
                                                )
import           Data.Maybe
import           Data.Ord                       ( Down(Down) )
import           Data.Pool                      ( withResource )
import           Data.Ratio                     ( (%) )
import           Data.Text                      ( Text )
import           Shared.Database                ( PooledDB )
import           Shared.Models.Currency         ( Currency(..)
                                                , currencyToDouble
                                                , getMonetaryValue
                                                , roundUpUSD
                                                )
import           Shared.Models.Group
import           Shared.Models.Ids              ( RewardId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common
import           Shared.Models.Ledger.Entry
import           Shared.Models.Rewards.Boost    ( RewardBoost(..) )
import           Shared.Models.Rewards.Categorizer
                                                ( categorize )
import           Shared.Models.RiskScore
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserModel(usrFirstName) )

matchRID :: [RewardBoost] -> Maybe RewardId -> Maybe RewardBoost
matchRID allRewards (Just r) = find (\boost -> boostId boost == r) allRewards
matchRID _          Nothing  = Nothing

nonDeclined :: [Transaction] -> [Transaction]
nonDeclined = filter
  (\t -> case (trxState t, trxDisplayAmount t) of
    (_           , Currency "USD" 0) -> False
    (TrxPending  , _               ) -> True
    (TrxCompleted, _               ) -> True
    (_           , _               ) -> False
  )

generateWeekly :: (MonadIO m) => PooledDB -> m Value
generateWeekly pool = do
  purchases  <- liftIO $ withResource pool $ getGlobalPurchasesInLast "7 day"
  allRewards <- liftIO $ withResource pool getAllRewards

  let grouped     = groupedByGID $ nonDeclined purchases
  let sumPerGroup = mapSnd (sum . fmap trxDisplayAmount) grouped
  let totalMedian = median $ fmap snd sumPerGroup
  let totalMean   = mean $ fmap snd sumPerGroup

  let rewards =
        sortAndGroupBy (\Transaction {..} -> trxRewardId)
          . nonDeclined
          $ purchases

  let namedRewards  = fmap (first $ matchRID allRewards) rewards
  let spentByReward = mapSnd (sum . fmap trxDisplayAmount) namedRewards

  return $ object
    [ "purchases" .= purchases
    , "grouped" .= grouped
    , "sumPerGroup" .= sumPerGroup
    , "totalMedian" .= totalMedian
    , "totalMean" .= totalMean
    , "spentByReward" .= spentByReward
    ]

boostReward :: Maybe RewardBoost -> Transaction -> Currency
boostReward Nothing Transaction {..} =
  roundUpUSD $ trxDisplayAmount * Currency "USD" 0.01
boostReward (Just RewardBoost {..}) Transaction {..} =
  roundUpUSD $ trxDisplayAmount * Currency "USD" (boostRewardInBips % 10000)

groupByRewards
  :: [RewardBoost] -> [Transaction] -> [(Maybe RewardBoost, [Transaction])]
groupByRewards allRewards = fmap (first $ matchRID allRewards)
  . sortAndGroupBy (\Transaction {..} -> trxRewardId)

categorizeTrx :: [RewardBoost] -> Transaction -> Maybe RewardBoost
categorizeTrx activeRewards t =
  listToMaybe
    . sortOn (Down . boostRewardInBips)
    . mapMaybe (categorize t)
    $ activeRewards

generateWeeklyActiveUsers
  :: (MonadIO m) => (UserID -> Value -> IO ()) -> Text -> PooledDB -> m ()
generateWeeklyActiveUsers trackEvent timeInterval pool = do
  activeUsers <- liftIO $ withResource pool getActiveUsers
  forM_ activeUsers $ \u -> do
    fullData <- generateWeeklyUser timeInterval u pool
    liftIO $ putStr "User report sent" >> print (u, timeInterval)
    liftIO $ trackEvent u fullData

generateWeeklyUser :: (MonadIO m) => Text -> UserID -> PooledDB -> m Value
generateWeeklyUser timeInterval uid pool = do
  -- generic
  allRewards <- liftIO $ withResource pool getAllRewards
  let activeRewards = filter boostActive allRewards

  globalPurchases <- liftIO $ withResource pool $ getGlobalPurchasesInLast
    timeInterval
  let globalRewardGroups = sortAndGroupBy (categorizeTrx activeRewards)
        $ nonDeclined globalPurchases
  let globalTopRewards =
        fmap
            (\(b, ts) ->
              ( boostName b
              , (roundUpUSD . sum . fmap trxDisplayAmount) ts
              , length ts
              )
            )
          . take 5
          . sortOn (Down . \(_, ts) -> length ts)
          . foldr (\(mb, ts) xs -> [ (fromJust mb, ts) | isJust mb ] <> xs) []
          $ globalRewardGroups
  let globalMean =
        roundUpUSD
          . mean
          . fmap snd
          . mapSnd (sum . fmap trxDisplayAmount)
          . groupedByGID
          . nonDeclined
          $ globalPurchases

  -- user based ones
  userRisk      <- liftIO $ withResource pool $ getCurrentRiskForUser uid
  userPurchases <- liftIO $ withResource pool $ getUserPurchasesInLast
    uid
    timeInterval
  userLedgerEntries <- liftIO $ withResource pool $ getNewRewardJournalsForUser
    uid
    timeInterval

  let currentRiskScore = fromMaybe exampleRiskScore userRisk
  let dollarToRiskUp   = dollarToNextLevel currentRiskScore

  -- userJournals <- liftIO $ withResource pool $ getUserJournals uid
  let namedRewards     = groupByRewards allRewards $ nonDeclined userPurchases
  let spentByReward =
        sortOn (Down . snd)
          . mapSnd (sum . fmap trxDisplayAmount)
          $ namedRewards
  let earnedByReward =
        sortOn (Down . snd)
          . fmap (\(b, ts) -> (b, sum $ fmap (boostReward b) ts))
          $ namedRewards
  let sumRewardsEarned = sum $ fmap snd earnedByReward

  let maxRewards = (\t -> (t, categorizeTrx activeRewards t))
        <$> nonDeclined userPurchases
  let maxEarned = fmap
        (\(t, b) ->
          ( boostId <$> b
          , roundUpUSD $ trxDisplayAmount t * Currency
            "USD"
            (maybe 100 boostRewardInBips b % 10000)
          , trxDisplayAmount t
          )
        )
        maxRewards
  let fstOThr (x, _, _) = x
  let sndOThr (_, x, _) = x
  let thrOThr (_, _, x) = x
  let potentialMaxPurchases =
        groupBy (\a b -> fstOThr a == fstOThr b) . sortOn fstOThr $ maxEarned
  let potentialMaxByReward =
        sortOn (Down . snd)
          . fmap
              (second sum . \rs ->
                ( maybe "Everything else" boostName
                  $ matchRID allRewards (fstOThr $ head rs)
                , fmap sndOThr rs
                )
              )
          $ potentialMaxPurchases
  let potentialMaxSpent =
        sortOn (Down . snd)
          . fmap
              (second sum . \rs ->
                ( maybe "Everything else" boostName
                  $ matchRID allRewards (fstOThr $ head rs)
                , fmap thrOThr rs
                )
              )
          $ potentialMaxPurchases
  let sumMaxEarned = sum $ fmap (\(_, s, _) -> s) maxEarned
  let percentOfMaxEarned :: Int =
        ceiling
          $ ( getMonetaryValue sumRewardsEarned
            / max 1 (getMonetaryValue sumMaxEarned)
            )
          * 100

  let totalSpent = sum . fmap trxDisplayAmount . nonDeclined $ userPurchases
  let spentByPurchaserId =
        sortOn (Down . snd)
          . mapSnd (sum . fmap trxDisplayAmount)
          . sortAndGroupBy (\Transaction {..} -> trxUserId)
          . nonDeclined
          $ userPurchases
  spentByPurchaser <- forM spentByPurchaserId
    $ \(pid, c) -> (, c) <$> liftIO (withResource pool $ getJustUser pid)

  let rewardsEarned = sum $ fmap (factAmount . lenFact) userLedgerEntries

  return $ object
    [ "spentSum" .= currencyToDouble totalSpent
    , "spentGroupedPurchaser"
      .= fmap (bimap usrFirstName currencyToDouble) spentByPurchaser
    , "spentGroupedReward" .= fmap (second currencyToDouble) potentialMaxSpent
    , "spentSumGroupedReward"
      .= fmap (bimap (maybe "Everything else" boostName) currencyToDouble)
              spentByReward
    , "rewardSumGrouped"
      .= fmap (bimap (maybe "Everything else" boostName) currencyToDouble)
              earnedByReward
    , "purchases" .= fmap
      (\t@Transaction {..} ->
        ( fromMaybe "Purchase" trxDescription
        , currencyToDouble trxDisplayAmount
        , trxPurchasedAt
        , maybe "Everything else" boostName $ categorizeTrx activeRewards t
        )
      )
      (nonDeclined userPurchases)
    , "rewardPotentialSum"
      .= currencyToDouble (max sumMaxEarned sumRewardsEarned)
    , "rewardSum" .= currencyToDouble sumRewardsEarned
    , "rewardPotentialSumGrouped"
      .= fmap (second currencyToDouble) potentialMaxByReward
    , "globalTopRewards" .= fmap fstOThr globalTopRewards
    , "globalMean" .= currencyToDouble globalMean
    , "rewardPotentialPercentEarned" .= percentOfMaxEarned
    , "riskCurrentScore" .= rskTrustScore currentRiskScore
    , "riskDollarToUp" .= currencyToDouble dollarToRiskUp
    , "riskCurrentLimit"
      .= currencyToDouble (riskAdjustedLimit currentRiskScore)
    , "riskNextLimit" .= currencyToDouble
      (riskAdjustedLimit currentRiskScore
        { rskTrustScore = rskTrustScore currentRiskScore + 10
        }
      )
    , "rewardEarnedPersonally" .= currencyToDouble rewardsEarned
    ]

generateEarnedRewards :: (MonadIO m) => Text -> PooledDB -> m Value
generateEarnedRewards interval pool = do
  allRewards      <- liftIO $ withResource pool getAllRewards

  entries         <- liftIO $ withResource pool $ getNewRewardJournals interval
  globalPurchases <- liftIO $ withResource pool $ getGlobalPurchasesInLast
    interval

  let ledgerByUser = sortAndGroupBy lenUser entries
  let _factsByUser = fmap (second (fmap lenFact)) ledgerByUser
  let earnedByUser =
        fmap (second (sum . fmap (factAmount . lenFact))) ledgerByUser

  let nonDeclinedPurchases = nonDeclined globalPurchases
  purchaseByUserList <- forM nonDeclinedPurchases $ \Transaction {..} ->
    case trxGroupId of
      Nothing             -> return []
      Just (groupID, rev) -> do
        group <- liftIO $ withResource pool $ getJustGroupByIdAndRev
          (groupID, rev)
        case group of
          Nothing                      -> return []
          Just GroupModel { grpSplit } -> do
            let reward =
                  maybe 100 boostRewardInBips
                    . lookup trxRewardId
                    . fmap (\b -> (Just $ boostId b, b))
                    $ allRewards
            forM grpSplit $ \GroupSplit {..} -> do
              let earned = roundUpUSD $ trxDisplayAmount * Currency
                    "USD"
                    ((splRatio / 100) * (reward % 10000))
              return (splUser, earned)

  let purchaseByUser =
        fmap (second (sum . fmap snd))
          . sortAndGroupBy fst
          . concat
          $ purchaseByUserList

  return $ object
    ["earnedByUser" .= earnedByUser, "purchaseByUser" .= purchaseByUser]
