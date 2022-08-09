{-# LANGUAGE NamedFieldPuns #-}

module Shared.Models.Rewards.Categorizer where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Shared.Models.Rewards.Boost    ( BoostMatcher(..)
                                                , RewardBoost
                                                  ( RewardBoost
                                                  , boostMatch
                                                  )
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )
type PercentAsInt = Integer

categorize :: Transaction -> RewardBoost -> Maybe RewardBoost
categorize trx r@RewardBoost { boostMatch } =
  if matchBoost trx boostMatch then Just r else Nothing

matchBoost :: Transaction -> BoostMatcher -> Bool
matchBoost Transaction { trxMerchant = Just CardMerchant { cmiMcc } } (MatchMCC mcc)
  = cmiMcc == mcc
matchBoost Transaction { trxMerchant = Nothing } (MatchMCC _) = False
matchBoost Transaction { trxMerchant = Just CardMerchant { cmiName } } (MatchText t)
  = t `matchText` cmiName
matchBoost Transaction { trxDescription = Just desc } (MatchText t) =
  t `matchText` desc
matchBoost Transaction{} (MatchText _) = False
matchBoost trx (MatchAND matches) = and $ fmap (matchBoost trx) matches
matchBoost trx (MatchOR matches) = or $ fmap (matchBoost trx) matches
matchBoost trx (MatchNOT matcher) = not $ matchBoost trx matcher

matchText :: Text -> Text -> Bool
matchText t1 t2 = T.toLower t1 `T.isInfixOf` T.toLower t2

matchAll :: BoostMatcher
matchAll = MatchNOT $ MatchMCC $ MastercardMCC "0000"
