{-# LANGUAGE RecordWildCards #-}

module Shared.Transactions.Categories.Cat006 where

import           Data.Text                      ( isInfixOf
                                                , toLower
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                )

-- brittany-next-binding --columns 100
-- | Category - Utilities, Internet, Cable, Phone and Insurnace
--  https://metabase.paytgthr.com/question/93/notebook
--  https://linear.app/paytgthr/issue/PAY-146#comment-3088f8cd
category006 :: Transaction -> Bool
category006 Transaction { trxMerchant = Nothing } = False
category006 Transaction { trxMerchant = Just CardMerchant {..} }
  | (cmiMcc == MastercardMCC "6300")
    || (cmiMcc == MastercardMCC "4812")
    || (cmiMcc == MastercardMCC "4814")
    || (cmiMcc == MastercardMCC "4900")
    || (cmiMcc == MastercardMCC "4899" && "at&t" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "comcast" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "fios" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "google fi" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "philo" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "select" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "spectrum" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "verizon" `isInfixOf` toLower cmiName)
    || (cmiMcc == MastercardMCC "4899" && "suddenlink" `isInfixOf` toLower cmiName)
  = True
  | otherwise
  = False
