{- HLINT ignore "Redundant do" -}
{-# LANGUAGE OverloadedStrings #-}

module LandingPage.Handlers.Transactions.Recomendations where

import           Text.Blaze.Html5               ( (!) )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

recomendation1 :: H.Html
recomendation1 = do
  H.div ! A.class_ "empty-recomendation" $ do
    "Why not switch your "
    H.a
      ! A.href "https://www.netflix.com/youraccountpayment"
      $ "Netflix subscription"
    " to your Tgthr card?"

recomendation2 :: H.Html
recomendation2 = do
  H.div ! A.class_ "empty-recomendation" $ do
    "Why not switch your "
    H.a ! A.href "https://secure.hulu.com/account" $ "Hulu subscription"
    " to your Tgthr card?"

recomendation3 :: H.Html
recomendation3 = do
  H.div ! A.class_ "empty-recomendation" $ do
    "Why not switch your utilities over to your Tgthr card?"

recomendation4 :: H.Html
recomendation4 = do
  H.div ! A.class_ "empty-recomendation" $ do
    "Why not grab dinner tonight on your Tgthr card?"

recomendation5 :: H.Html
recomendation5 = do
  H.div ! A.class_ "empty-recomendation" $ do
    "Why not switch you Postmates over to your Tgthr card?"

recomendation6 :: H.Html
recomendation6 = do
  H.div ! A.class_ "empty-recomendation" $ do
    "Why not buy some groceries on your Tgthr card?"
