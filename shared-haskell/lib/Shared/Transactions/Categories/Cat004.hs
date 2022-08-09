{-# LANGUAGE RecordWildCards #-}

module Shared.Transactions.Categories.Cat004 where

import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )

-- | Category - Pet Food and Supplies
--  https://metabase.paytgthr.com/question/94/notebook
category004 :: Transaction -> Bool
category004 Transaction { trxMerchant = Nothing } = False
category004 Transaction { trxMerchant = Just CardMerchant {..} }
  | cmiMcc == MastercardMCC "5995" = True
  | otherwise                      = False
