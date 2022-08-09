{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module Chewpaca.Web.Transaction where

import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , indigoColorButton
                                                , mediumButton
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( showCurr
                                                , showDateTime
                                                )
import           Chewpaca.Web.Transactions      ( extTrxId
                                                , renderGroupDetails
                                                , renderTransactionTable
                                                , renderTrxAdjustment
                                                , renderTrxMerchant
                                                , renderTrxSource
                                                , renderTrxSourceEvent
                                                , renderTrxSplitAmounts
                                                , renderTrxStateBubble
                                                , renderTrxTransactionDetails
                                                )
import           Chewpaca.Web.Users             ( generateLinkToUserPage
                                                , renderUserID
                                                )
import           Chewpaca.Web.Utils             ( uuidToStrStart )
import           Data.Maybe                     ( fromMaybe )
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( toText )
import           Shared.Models.Currency         ( getMonetaryValue )
import           Shared.Models.Ids              ( RewardId(RewardId)
                                                , TransactionId(TransactionId)
                                                )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionDetails(..)
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTransaction :: [(Transaction, UTCTime)] -> Html
renderTransaction []                       = "Error: No transactions to show"
renderTransaction trxs@(mostRecentRev : _) = do
  H.div ! A.id "top" $ mempty

  renderTransactionCard (fst mostRecentRev)

  H.div $ do
    H.h3 ! A.id "revisions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Revisions"
    renderTransactionTable trxs

  H.div $ do
    H.h3 ! A.id "actions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "User Actions"
    renderActions (fst mostRecentRev)

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTransactionCard :: Transaction -> Html
renderTransactionCard Transaction {..} = do
  let cardLeftText  = "text-sm leading-5 font-medium text-gray-500"
      cardRightText = "mt-1 text-sm leading-5 text-gray-900 sm:mt-0 sm:col-span-2"
      cardDataRow   = "mt-8 sm:mt-0 sm:grid sm:grid-cols-3 sm:gap-4 sm:border-t sm:border-gray-200 sm:px-6 sm:py-5"
  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-lg" $ do
    H.div ! A.class_ "px-4 py-5 border-b border-gray-200 sm:px-6" $ do
      H.h3 ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
        toHtml $ fromMaybe "" trxDescription
      H.p ! A.class_ "mt-1 max-w-2xl text-sm leading-5 text-gray-500" $ do
        if getMonetaryValue trxDisplayAmount >= 0
          then do
            toHtml $ showCurr trxDisplayAmount
          else do
            H.div ! A.class_ "text-red-600" $ toHtml $ showCurr trxDisplayAmount
            H.div $ do
              H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Credit Trx"
    H.div ! A.class_ "px-4 py-5 sm:p-0" $ do
      H.dl $ do
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Id"
          H.dd ! A.class_ cardRightText $ do
            extTrxId trxId
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Revision"
          H.dd ! A.class_ cardRightText $ do
            toHtml trxRevision
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "State"
          H.dd ! A.class_ cardRightText $ do
            renderTrxStateBubble trxState
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Description"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ fromMaybe "" trxDescription
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Source"
          H.dd ! A.class_ cardRightText $ do
            renderTrxSource trxSource
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Source Id"
          H.dd ! A.class_ cardRightText $ do
            toHtml trxSourceId
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Source event"
          H.dd ! A.class_ cardRightText $ do
            renderTrxSourceEvent trxSourceEvent
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "User"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href (generateLinkToUserPage trxUserId) ! A.class_ tableLinkClasses $ do
              renderUserID trxUserId
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Amount"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ showCurr trxDisplayAmount
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Billing amounts"
          H.dd ! A.class_ cardRightText $ do
            mapM_
              (\(amount, time) -> do
                H.div $ do
                  H.span $ showDateTime time
                  H.span " "
                  H.span $ toHtml $ showCurr amount
              )
              trxBillingAmounts
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Purchased at"
          H.dd ! A.class_ cardRightText $ do
            showDateTime trxPurchasedAt
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Details"
          H.dd ! A.class_ cardRightText $ do
            foldMap renderTrxTransactionDetails trxDetails
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Context"
          H.dd ! A.class_ cardRightText $ do
            case trxDetails of
              Nothing                   -> mempty
              Just CardTransaction {..} -> toHtml pcpContext
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Context"
          H.dd ! A.class_ cardRightText $ do
            case trxDetails of
              Just CardTransaction { pcpDescription = Just t } -> toHtml t
              _ -> mempty
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Group"
          H.dd ! A.class_ cardRightText $ do
            renderGroupDetails trxGroupId
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Source Idempotency"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ fromMaybe "" trxSourceIdempotency
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Split"
          H.dd ! A.class_ cardRightText $ do
            renderTrxSplitAmounts trxSplitAmounts
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Merchant"
          H.dd ! A.class_ cardRightText $ do
            renderTrxMerchant trxMerchant
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Adjustments"
          H.dd ! A.class_ cardRightText $ do
            renderTrxAdjustment trxAdjustments
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Reward"
          H.dd ! A.class_ cardRightText $ do
            case trxRewardId of
              Nothing           -> "No Reward"
              Just (RewardId u) -> toHtml $ uuidToStrStart u

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderActions :: Transaction -> Html
renderActions Transaction { trxId = TransactionId t } = do
  let tid = toValue $ toText t

  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-md" $ do
    H.ul $ do
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Set transaction to completed"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/transaction/" <> tid <> "/set/state/trxcompleted"
              H.form ! A.action url ! A.method "post" $ do
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Set to Completed"
