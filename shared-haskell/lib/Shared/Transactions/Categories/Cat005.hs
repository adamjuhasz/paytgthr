{-# LANGUAGE RecordWildCards #-}

module Shared.Transactions.Categories.Cat005 where

import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )

-- | Category - Restaurants, Takeout & Bars
--  https://metabase.paytgthr.com/question/95/notebook
category005 :: Transaction -> Bool
category005 Transaction { trxMerchant = Nothing } = False
category005 Transaction { trxMerchant = Just CardMerchant {..} }
  | (cmiMcc == MastercardMCC "5812")
    || (cmiMcc == MastercardMCC "5813")
    || (cmiMcc == MastercardMCC "5814")
  = True
  | otherwise
  = False
