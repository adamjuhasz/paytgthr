{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Transactions.Renderers.EmptyTransaction where

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

emptyTransaction :: H.Html -> H.Html
emptyTransaction recomendation = do
  H.div ! A.class_ "categories" $ do
    -- list of categories
    mapM_ ((H.div ! A.class_ "category") . H.preEscapedText)
          ["&#128661;", "&#129361;", "&#128688;", "&#128717", "&#127869;"]
  -- text 
  H.div ! A.class_ "empty-explainer" $ "You don't have any transactions yet"
  -- recoemndation
  recomendation
