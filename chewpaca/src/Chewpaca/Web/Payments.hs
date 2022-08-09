{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.Payments where

import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , dataTable
                                                , tableCellClasses
                                                , tableFirstCellClasses
                                                , tableHeaderCellClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( showCurr
                                                , showDateTime
                                                )
import           Chewpaca.Web.Users             ( generateLinkToUserPage
                                                , renderUserID
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.UUID                      ( toText )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentFailureCode(..)
                                                , PaymentId(..)
                                                , PaymentStatus(..)
                                                , PaymentSubType(..)
                                                , PaymentType(..)
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , AttributeValue
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

renderPaymentId :: PaymentId -> Html
renderPaymentId (PaymentId p) = case T.splitOn "-" $ toText p of
  []             -> "Error: Can't split pid"
  (firstSec : _) -> toHtml (firstSec <> "...")

generateLinkToPayment :: PaymentId -> AttributeValue
generateLinkToPayment (PaymentId p) = toValue $ "/payment/" <> toText p

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderPaymentStateBubble :: PaymentStatus -> Html
renderPaymentStateBubble PaymentCreated    = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Payment Created"
renderPaymentStateBubble PaymentPending    = H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Payment Pending"
renderPaymentStateBubble PaymentCompleted  = H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ "Payment Completed"
renderPaymentStateBubble PaymentCancelled  = H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Payment Canceled"
renderPaymentStateBubble (PaymentFailed r) = do
  H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Payment Failed"
  case r of
    ACHUnknown _ -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Unknown"
    x            -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ toHtml $ show x

  case r of
    ACHR01        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Insufficient Funds"
    ACHR02        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Account Closed"
    ACHR03        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Unable to Locate Account"
    ACHR04        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Invalid Account Number"
    ACHR05        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Unauthorized Debit Entry"
    ACHR06        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Returned per ODFI’s Request"
    ACHR07        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Authorization Revoked by Customer"
    ACHR08        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Payment Stopped"
    ACHR09        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Uncollected Funds"
    ACHR10        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Customer Advises Not Authorized"
    ACHR11        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Customer Advises Entry Not in Accordance"
    ACHR12        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Branch Sold to Another DFI"
    ACHR13        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "RDFI not qualified to participate"
    ACHR14        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Representative Payee Deceased"
    ACHR15        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Account Holder Deceased"
    ACHR16        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Account Frozen"
    ACHR17        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "File Record Edit Criteria"
    ACHR20        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Non-Transaction Account"
    ACHR21        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Invalid Company Identification"
    ACHR22        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Invalid Individual ID Number"
    ACHR23        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Credit Entry Refused by Receiver"
    ACHR24        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Duplicate Entry"
    ACHR29        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Corporate Customer Advises Not Authorized"
    ACHR31        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Permissible Return Entry (CCD and CTX only)"
    ACHR33        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Return of XCK Entry"
    ACHUnknown "" -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Unknown Reason"
    ACHUnknown t  -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Reason: " <> toHtml t

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderPayTypeBubble :: PaymentType -> Html
renderPayTypeBubble DebitFromUser = H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Debit From"
renderPayTypeBubble CreditToUser  = H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Credit To"

extPayId :: PaymentId -> Text
extPayId (PaymentId p) = toText p

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderPaymentList :: [Payment] -> Html
renderPaymentList payments = do
  dataTable $ do
    H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
      H.thead $ do
        H.tr $ do
          H.th ! A.class_ tableHeaderCellClasses $ "Id"
          H.th ! A.class_ tableHeaderCellClasses $ "Rev"
          H.th ! A.class_ tableHeaderCellClasses $ "User"
          H.th ! A.class_ tableHeaderCellClasses $ "State"
          H.th ! A.class_ tableHeaderCellClasses $ "Amount"
          H.th ! A.class_ tableHeaderCellClasses $ "Direction"
          H.th ! A.class_ tableHeaderCellClasses $ "Type"
          H.th ! A.class_ tableHeaderCellClasses $ "Last updated"
      H.tbody $ do
        let createRow Payment {..} = do
              H.tr ! A.id (toValue $ "payment-" <> extPayId payId) ! A.class_ "bg-white" $ do
                H.td ! A.class_ tableFirstCellClasses $ do
                  H.a ! A.href (generateLinkToPayment payId) ! A.class_ tableLinkClasses $ do
                    renderPaymentId payId
                H.td ! A.class_ tableCellClasses $ do
                  toHtml payRevision
                H.td ! A.class_ tableCellClasses $ do
                  H.a ! A.href (generateLinkToUserPage payUser) ! A.class_ tableLinkClasses $ do
                    renderUserID payUser
                H.td ! A.class_ tableCellClasses $ do
                  renderPaymentStateBubble payStatus
                H.td ! A.class_ tableCellClasses $ do
                  toHtml $ showCurr payAmount
                H.td ! A.class_ tableCellClasses $ do
                  renderPayTypeBubble payType
                H.td ! A.class_ tableCellClasses $ do
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

                H.td ! A.class_ tableCellClasses $ do
                  showDateTime payCreatedAt

        mapM_ createRow payments
