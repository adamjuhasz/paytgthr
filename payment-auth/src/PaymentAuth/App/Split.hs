{-# LANGUAGE DataKinds #-}

module PaymentAuth.App.Split where

import           Data.Decimal                   ( allocate
                                                , roundTo
                                                )

calculateSplit :: Rational -> [(a, Rational)] -> [(a, Rational)]
calculateSplit amount groupSplit = zip justUsers owedRationals
 where
  decimalAmount = roundTo 2 $ fromRational amount
  justSplits    = fmap (floor . snd) groupSplit
  justUsers     = fmap fst groupSplit
  owedDecimals  = allocate decimalAmount justSplits
  owedRationals = fmap toRational owedDecimals
