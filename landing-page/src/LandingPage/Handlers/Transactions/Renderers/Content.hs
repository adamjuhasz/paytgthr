{- HLINT ignore "Redundant do" -}
{-# LANGUAGE RecordWildCards, OverloadedStrings, StrictData #-}

module LandingPage.Handlers.Transactions.Renderers.Content where

import           Data.Text                      ( Text )
import           LandingPage.Handlers.Transactions.Renderers.EmptyTransaction
                                                ( emptyTransaction )
import           LandingPage.Handlers.Transactions.Renderers.SafeSpend
                                                ( safeToSpend )
import           LandingPage.Handlers.Transactions.Renderers.Transaction
                                                ( transactionDetails )
import           LandingPage.Handlers.Transactions.Types
                                                ( TrxTemplate(..)
                                                , calculateNextLink
                                                , calculatePrevLink
                                                , nextColor
                                                , prevColor
                                                , trxColors
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

data FrameSide = LeftSide | RightSide

sideFrame :: FrameSide -> H.Html -> H.Html
sideFrame side = do
  let textSide = case side of
        LeftSide  -> "left"
        RightSide -> "right"
  H.div ! A.class_ (textSide <> "-frame")

sideFrameEmpty :: FrameSide -> H.AttributeValue -> H.Html
sideFrameEmpty side prevLink = do
  let state = case side of
        LeftSide  -> "prev"
        RightSide -> "next"
      classes = "purple-grey transaction-" <> state
  sideFrame side $ do
    H.a ! A.href prevLink ! A.class_ classes $ ""

sideFrameNil :: FrameSide -> H.AttributeValue -> H.Html
sideFrameNil side linkTo = do
  let state = case side of
        LeftSide  -> "prev"
        RightSide -> "next"
      classes = "purple-grey transaction-" <> state
  sideFrame side $ do
    H.a ! A.href linkTo ! A.class_ classes $ ""

sideFramePrev :: FrameSide -> H.AttributeValue -> Text -> H.Html
sideFramePrev side linkTo currColor = do
  let theColor = case side of
        LeftSide  -> prevColor currColor
        RightSide -> nextColor currColor
      state = case side of
        LeftSide  -> "prev"
        RightSide -> "next"
      classes = "transaction-" <> state <> " " <> toValue theColor
  sideFrame side $ do
    H.a ! A.href linkTo ! A.class_ classes $ ""

-- inline brittany config for width
-- brittany-next-binding --columns 500
blazeRenderer :: TrxTemplate -> H.Html
blazeRenderer template = do
  let colorTrx = toValue $ case template of
        EmptyTransaction{}       -> trxColors !! 1
        SafeSpend{}              -> trxColors !! 1
        TransactionTemplate {..} -> color
      finalColor     = colorTrx <> "-transaction"
      currentClasses = case template of
        EmptyTransaction{}       -> "empty-state purple-grey"
        TransactionTemplate {..} -> toValue color
        SafeSpend{}              -> "purple-grey"
      prevLink = calculatePrevLink template
      nextLink = calculateNextLink template

  H.a ! A.href prevLink ! A.class_ "transaction-arrow left-arrow" ! A.id "trx-prev-link" $ do
    H.img ! A.src "/images/app/left_arrow.svg"

  H.div ! A.class_ "transaction-frame" $ do
    case template of
      EmptyTransaction{}    -> sideFrameEmpty LeftSide prevLink
      TransactionTemplate { prevTransaction = Just _, ..} -> sideFramePrev LeftSide prevLink color
      TransactionTemplate{} -> sideFrameNil LeftSide prevLink
      SafeSpend{}           -> return ()

    H.div ! A.class_ ("transaction-bubble " <> finalColor) $ do
      H.div ! A.class_ "app-header" $ do
        H.span "Transactions"

      H.div ! A.class_ ("transaction " <> currentClasses) ! A.id "current-transaction" $ do
        case template of
          EmptyTransaction {..}    -> emptyTransaction recomendation
          TransactionTemplate {..} -> transactionDetails user currentTransaction purchaser
          SafeSpend {..}           -> safeToSpend spendable accountName

      H.div ! A.class_ "app-header-spacer" $ ""

    case template of
      EmptyTransaction{}                     -> return ()
      TransactionTemplate { nextTransaction = Just _, ..} -> sideFramePrev RightSide nextLink color
      TransactionTemplate{}                  -> return ()
      SafeSpend { hasTransactions = Just _ } -> sideFramePrev RightSide nextLink "yellow"
      SafeSpend{}                            -> sideFrameNil RightSide nextLink

  H.a ! A.href nextLink ! A.class_ "transaction-arrow right-arrow" ! A.id "trx-next-link" $ do
    H.img ! A.src "/images/app/right_arrow.svg"
