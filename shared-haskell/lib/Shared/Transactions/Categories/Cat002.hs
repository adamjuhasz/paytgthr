{-# LANGUAGE RecordWildCards #-}

module Shared.Transactions.Categories.Cat002 where

import           Data.Text                      ( isInfixOf
                                                , toLower
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )

-- inline brittany config for width
-- brittany-next-binding --columns 100
-- | Category - Grocery & Big Box Stores
--  https://metabase.paytgthr.com/question/92/notebook
category002 :: Transaction -> Bool
category002 Transaction { trxMerchant = Nothing } = False
category002 Transaction { trxMerchant = Just CardMerchant {..} }
  | (cmiMcc == MastercardMCC "5411")
    || (cmiMcc == MastercardMCC "5310")
    || (cmiMcc == MastercardMCC "5300")
    || (cmiMcc == MastercardMCC "5331")
    || (cmiMcc == MastercardMCC "5422")
    || (cmiMcc == MastercardMCC "5968" && "dinnerly" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "5499" && "home chef" `isInfixOf` toLower cmiName)
  = True
  | otherwise
  = False
