{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module Chewpaca.Web.Transactions.Drawings where

import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.UUID                      ( toText )
import           Shared.Models.Currency
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionId(TransactionId)
                                                , TransactionState(TrxCompleted)
                                                )
import           System.Random                  ( randomRIO ) 
import           Text.Blaze.Html5               ( Html
                                                , (!)
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

renderNovemberGiveaway :: [Transaction] -> IO Html
renderNovemberGiveaway trxs = do
  let inNovember Transaction {..} =
        (formatTime defaultTimeLocale "%m" trxPurchasedAt == "11")
          && (trxState == TrxCompleted)
          && (trxDisplayAmount /= Currency "USD" 0)
  let novemberTrxs = filter inNovember trxs
  let replicated = concatMap
        (\t -> replicate (ceiling . getMonetaryValue $ trxDisplayAmount t) t)
        novemberTrxs

  x <- randomRIO (0, length replicated - 1)

  return $ do
    H.h3 "November drawing"
    H.div $ do
      "Number of trxs: "
      toHtml $ length novemberTrxs
    H.div $ do
      "Total raffle tickets: "
      toHtml $ length replicated
    H.div $ do
      let randomDrawing = replicated !! x
      let getId Transaction { trxId = TransactionId tid } = toText tid
      let theId = getId randomDrawing
      H.a ! A.href (toValue $ "/transaction/" <> theId) $ do
        "Winner: (clickable)" <> toHtml theId
