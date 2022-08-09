{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings, StrictData #-}

module LandingPage.Handlers.Transactions.Renderers.SafeSpend where

import           Data.Text                      ( Text )
import           Text.Blaze.Html5               ( (!)
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

safeToSpend :: Double -> Text -> H.Html
safeToSpend balance accountName = do
  let isHidden = A.style "visibility: hidden;"
      rounded  = round balance :: Int
  H.div ! A.class_ "category" $ H.preEscapedText "&#128178;"
  H.div ! A.class_ "category-details-spacer" $ ""
  H.div ! A.class_ "details" ! isHidden $ do
    H.div ! A.class_ "d-cell date" $ ""
    H.div ! A.class_ "details-spacer" $ "|"
    H.div ! A.class_ "d-cell cardholder" $ ""
  H.div ! A.class_ "details-merchant-spacer" $ ""
  H.div ! A.class_ "merchant" $ "Max purchase possible"
  H.div ! A.class_ "merchant-price-spacer" $ ""
  H.div ! A.class_ "price" ! A.id "price-group" $ do
    H.div ! A.class_ "currency" $ "$"
    H.div ! A.class_ "amount" $ toHtml rounded
    H.div ! A.class_ "amount-detail" $ ""
  H.div ! A.class_ "price-share-spacer" $ ""
  H.div ! A.class_ "share" ! A.id "share-group" $ toHtml accountName
  H.div ! A.class_ "share-split-spacer" $ ""
  H.div ! A.class_ "split" ! isHidden $ "0:0"
