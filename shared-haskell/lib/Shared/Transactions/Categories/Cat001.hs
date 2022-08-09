{-# LANGUAGE RecordWildCards #-}

module Shared.Transactions.Categories.Cat001 where

import           Data.Text                      ( isInfixOf
                                                , toLower
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )

-- | Category - Gas Stations & Transportation
--  https://metabase.paytgthr.com/question/88/notebook
--  https://linear.app/paytgthr/issue/PAY-146#comment-07772f34
category001 :: Transaction -> Bool
category001 Transaction { trxMerchant = Nothing } = False
category001 Transaction { trxMerchant = Just CardMerchant {..} }
  | (cmiMcc == MastercardMCC "4121")
    || (cmiMcc == MastercardMCC "5541")
    || (cmiMcc == MastercardMCC "5542")
    || (cmiMcc == MastercardMCC "5542")
    || (cmiMcc == MastercardMCC "5499" && "wawa" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "5499" && "valero" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "5499" && "7-11" `isInfixOf` toLower cmiName)
  = True
  | otherwise
  = False
