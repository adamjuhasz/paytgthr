{-# LANGUAGE RecordWildCards #-}

module Shared.Transactions.Categories.Cat003 where

import           Data.Text                      ( isInfixOf
                                                , toLower
                                                )
import           Shared.Models.Transaction      ( MerchantInfo(..)
                                                , Transaction(..)
                                                )

-- | Category - Media Subscriptions
--  https://metabase.paytgthr.com/question/90
category003 :: Transaction -> Bool
category003 Transaction { trxMerchant = Nothing } = False
category003 Transaction { trxMerchant = Just CardMerchant {..} }
  | ("netflix" `isInfixOf` toLower cmiName)
    || ("hulu" `isInfixOf` toLower cmiName)
    || ("spotify" `isInfixOf` toLower cmiName)
    || ("cbs all" `isInfixOf` toLower cmiName)
    || ("peacock" `isInfixOf` toLower cmiName)
    || ("starz" `isInfixOf` toLower cmiName)
    || ("bet plus" `isInfixOf` toLower cmiName)
    || ("disney plus" `isInfixOf` toLower cmiName)
    || ("disney+" `isInfixOf` toLower cmiName)
    || ("hbo" `isInfixOf` toLower cmiName)
    || ("showtime" `isInfixOf` toLower cmiName)
    || ("apple tv" `isInfixOf` toLower cmiName)
    || ("paramount plus" `isInfixOf` toLower cmiName)
    || ("paramount+" `isInfixOf` toLower cmiName)
    || ("sling tv" `isInfixOf` toLower cmiName)
    || ("youtubemusic" `isInfixOf` toLower cmiName)
    || ("youtube tv" `isInfixOf` toLower cmiName)
    || ("pure flix" `isInfixOf` toLower cmiName)
    || ("roku" `isInfixOf` toLower cmiName)
  = True
  | otherwise
  = False
