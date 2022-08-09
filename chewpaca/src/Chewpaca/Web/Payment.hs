{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.Payment where

import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( showCurr
                                                , showDateTime
                                                )
import           Chewpaca.Web.Payments          ( extPayId
                                                , renderPayTypeBubble
                                                , renderPaymentList
                                                , renderPaymentStateBubble
                                                )
import           Chewpaca.Web.Users             ( extUserID
                                                , generateLinkToUserPage
                                                )
import           Data.UUID                      ( toText )
import           Shared.Models.Ids              ( JournalId(JournalId) )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentMethod(..)
                                                , PaymentSubType(..)
                                                )
import           Shared.Models.User             ( RedactedText(..) )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderPayment :: [Payment] -> Html
renderPayment []   = "Error: no Payment Revs to show"
renderPayment revs = do
  H.div ! A.id "top" $ mempty

  renderPaymentCard (head revs)

  H.div $ do
    H.h3 ! A.id "revisions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Revisions"
    renderPaymentList revs

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderPaymentCard :: Payment -> Html
renderPaymentCard Payment {..} = do
  let cardLeftText  = "text-sm leading-5 font-medium text-gray-500"
      cardRightText = "mt-1 text-sm leading-5 text-gray-900 sm:mt-0 sm:col-span-2"
      cardDataRow   = "mt-8 sm:mt-0 sm:grid sm:grid-cols-3 sm:gap-4 sm:border-t sm:border-gray-200 sm:px-6 sm:py-5"
  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-lg" $ do
    H.div ! A.class_ "px-4 py-5 sm:p-0" $ do
      H.dl $ do
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Id"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ extPayId payId
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Revisions"
          H.dd ! A.class_ cardRightText $ do
            toHtml payRevision
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "User"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href (generateLinkToUserPage payUser) ! A.class_ tableLinkClasses $ do
              extUserID payUser
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Initiated On"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href (generateLinkToUserPage payUser) ! A.class_ tableLinkClasses $ do
              showDateTime payCreatedAt
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Status"
          H.dd ! A.class_ cardRightText $ do
            renderPaymentStateBubble payStatus
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Direction"
          H.dd ! A.class_ cardRightText $ do
            renderPayTypeBubble payType
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Amount"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ showCurr payAmount
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Visible"
          H.dd ! A.class_ cardRightText $ do
            case paySubType of
              InitialVerification -> do
                H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800 mr-1") $ "Initial Verification"
                H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800 mr-1") $ "Hidden"
              RefundVerification -> do
                H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800 mr-1") $ "Refund Verification"
                H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800 mr-1") $ "Hidden"
              NormalPayment -> do
                H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800 mr-1") $ "NormalPayment"
                H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800 mr-1") $ "Visible"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Method"
          H.dd ! A.class_ cardRightText $ do
            case payMethod of
              DwollaSettlement -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "DwollaSettlement"
              DwollaACH _      -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "DwollaACH"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Method Id"
          H.dd ! A.class_ cardRightText $ do
            case payMethodId of
              Nothing -> mempty
              Just t  -> toHtml t
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "ACH Info"
          H.dd ! A.class_ cardRightText $ do
            case payACHInfo of
              Nothing -> H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Unknown"
              Just (RedactedText routing, RedactedText account) -> do
                H.div $ do
                  "Routing: " <> toHtml routing
                H.div $ do
                  "Account: " <> toHtml account
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Sub Type"
          H.dd ! A.class_ cardRightText $ do
            case paySubType of
              InitialVerification -> "Initial Verification"
              RefundVerification  -> "Refund Verification"
              NormalPayment       -> "Normal Payment"

        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "From Journal"
          H.dd ! A.class_ cardRightText $ do
            case payFromJournal of
              Nothing              -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "No Journal"
              Just (JournalId jid) -> toHtml $ toText jid

        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "To Journal"
          H.dd ! A.class_ cardRightText $ do
            case payToJournal of
              Nothing              -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "No Journal"
              Just (JournalId jid) -> toHtml $ toText jid
