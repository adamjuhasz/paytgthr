{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.Transactions where

import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( renderMessageLink
                                                , showCurr
                                                , showDateTime
                                                )
import           Chewpaca.Web.Groups            ( generateLinkToGroup
                                                , renderGroupId
                                                )
import           Chewpaca.Web.Users             ( generateLinkToUserPage
                                                , renderUserID
                                                )
import           Control.Monad                  ( when )
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( toText )
import           Shared.Models.Base             ( Revision )
import           Shared.Models.Currency         ( getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.Group            ( GroupId(..) )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , AptoAdjustmentID(..)
                                                , AptoAdjustmentType(..)
                                                , CardNetwork(..)
                                                , DeclineReason(..)
                                                , MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , PurchaseType(..)
                                                , Transaction(..)
                                                , TransactionDetails(..)
                                                , TransactionEvent(..)
                                                , TransactionId(..)
                                                , TransactionSource(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( UserID(..) )
import           Text.Blaze.Html5               ( (!)
                                                , AttributeValue
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

renderTransactionId :: TransactionId -> Html
renderTransactionId (TransactionId t) = case T.splitOn "-" $ toText t of
  []             -> "Error: Can't split tid"
  (firstSec : _) -> toHtml (firstSec <> "...")

generateLinkToTransaction :: TransactionId -> AttributeValue
generateLinkToTransaction (TransactionId t) =
  toValue $ "/transaction/" <> toText t

extTrxId :: TransactionId -> Html
extTrxId (TransactionId t) = toHtml $ toText t

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTrxStateBubble :: TransactionState -> Html
renderTrxStateBubble TrxCreated           = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Trx Created"
renderTrxStateBubble TrxAuthorized        = H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Trx Authed"
renderTrxStateBubble TrxPending           = H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Trx Pending"
renderTrxStateBubble TrxPendingReversal   = H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800") $ "Trx Pending Reversal"
renderTrxStateBubble (TrxDeclined reason) = do
  H.div $ do
    H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1 mb-1") $ "Trx Declined"
  H.div $ case reason of
    LowBalance users -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Low Personal limit"
      H.br
      mapM_
        (\u -> H.a ! A.href (generateLinkToUserPage u) $ do
          H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800" <> " mr-1") $ renderUserID u
        )
        users
    BalanceCheckFail users -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Balance Checked failed"
      H.br
      mapM_
        (\u -> H.a ! A.href (generateLinkToUserPage u) $ do
          H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800" <> " mr-1") $ renderUserID u
        )
        users
    ExceedMaxTrxAmount amount -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Exceed max amount"
      H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml $ showCurr amount
    InvalidMerchant         -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Invalid merchant"
    LostorStolenCard        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Lost or stolen card"
    InvalidAmount           -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Invalid amount"
    InvalidCardNumber       -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Invalid card number"
    CardExpired             -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Card expired"
    SuspectFraud            -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "SuspectFraud"
    InternationalNotAllowed -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "InternationalNotAllowed"
    PinRetriesExceeded      -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "PinRetriesExceeded"
    IncorrectCVV            -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "IncorrectCVV"
    CardNotActivated        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "CardNotActivated"
    CardInactive            -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "CardInactive"
    CardClosed              -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "CardClosed"
    IncorrectAddress        -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "IncorrectAddress"
    IncorrectPin            -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "IncorrectPin"
    GroupError              -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "GroupError"
    UserNotFound    _       -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "UserNotFound"
    PaymentUnlinked users   -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Payment unlinked"
      H.br
      mapM_
        (\u -> H.a ! A.href (generateLinkToUserPage u) $ do
          H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800" <> " mr-1") $ renderUserID u
        )
        users
    P2PNotAllowed       -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "P2PNotAllowed"
    RiskyMerchant       -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "RiskyMerchant"
    UserNotActive users -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "UserNotActive"
      H.br
      mapM_
        (\u -> H.a ! A.href (generateLinkToUserPage u) $ do
          H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800" <> " mr-1") $ renderUserID u
        )
        users
    Unknown "Transaction not Permitted to Acquirer/Terminal" -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "ATM withdrawal"
    Unknown t -> do
      H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-1") $ "Unknown"
      H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml t

renderTrxStateBubble TrxCompleted    = H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ "Trx Completed"
renderTrxStateBubble (TrxDisputed _) = H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Trx Disputed"

renderTrxSource :: TransactionSource -> Html
renderTrxSource Apto =
  H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Apto"
renderTrxSource PayWithPrivacy =
  H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Privacy"
renderTrxSource UnknownSource =
  H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Unknown"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTrxSourceEvent :: TransactionEvent -> Html
renderTrxSourceEvent AuthRequest             = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "AuthRequest"
renderTrxSourceEvent StateTransition         = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "StateTransition"
renderTrxSourceEvent Reversal                = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Reversal"
renderTrxSourceEvent Refund                  = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Refund"
renderTrxSourceEvent NetworkCredit           = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "NetworkCredit"
renderTrxSourceEvent NetworkDebit            = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "NetworkDebit"
renderTrxSourceEvent NonFinancialEvent       = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "NonFinancialEvent"
renderTrxSourceEvent BalanceInquiry          = H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "BalanceInquiry"
renderTrxSourceEvent UnknownTransactionEvent = H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Unknown"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTrxTransactionDetails :: TransactionDetails -> Html
renderTrxTransactionDetails CardTransaction {..} = do
  when pcpIsCardPresent   (H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "Card Present")
  when pcpIsOnline        (H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "Online")
  when pcpIsInternational (H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "International")
  when pcpIsEMV           (H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "EMV Chip")
  case pcpNetwork of
    Visa             -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "Visa"
    Mastercard       -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "Mastercard"
    UnknownNetwork t -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800" <> " mr-2") $ toHtml t
  case pcpType of
    Nothing        -> mempty
    Just Signature -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "Signature"
    Just Pin       -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-2") $ "Pin"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroupDetails :: Maybe (GroupId, Revision) -> Html
renderGroupDetails Nothing         = H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "No Group"
renderGroupDetails (Just (g, rev)) = do
  H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ toHtml rev
  H.span " "
  H.a ! A.href (generateLinkToGroup g) ! A.class_ tableLinkClasses $ do
    renderGroupId g

renderTrxSplitAmounts :: [(UserID, Rational)] -> Html
renderTrxSplitAmounts splits = do
  mapM_
    (\(u, split) -> H.div $ do
      H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ toHtml
        (floor split :: Int)
      H.span " "
      H.a ! A.href (generateLinkToUserPage u) ! A.class_ tableLinkClasses $ do
        renderUserID u
    )
    splits

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTrxMerchant :: Maybe MerchantInfo -> Html
renderTrxMerchant Nothing = mempty
renderTrxMerchant (Just CardMerchant { cmiMcc = MastercardMCC mcc, ..}) = do
  H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml mcc
  H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml cmiMccDesc
  H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml cmiName
  when (isJust cmiLocality) (H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml $ fromMaybe "" cmiLocality)
  when (isJust cmiRegion)   (H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800" <> " mr-1") $ toHtml $ fromMaybe "" cmiRegion)

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTrxAdjustment :: [AptoAdjustment] -> Html
renderTrxAdjustment adjs = do
  mapM_
    (\AptoAdjustment { adjId = AptoAdjustmentID aid, ..} -> do
      H.div $ toHtml aid
      H.div $ showDateTime adjCreatedAt
      H.div $ do
        H.span "Local: "
        H.span $ toHtml $ showCurr adjAmountLocal
      H.div $ do
        H.span "Billing: "
        H.span $ toHtml $ showCurr adjAmountBilling
      H.div $ do
        H.span "Trx Id: "
        H.span $ toHtml $ fromMaybe "No ID" adjFSTransactionId
      H.div $ do
        case adjType of
          CaptureAdjustment       -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Capture Adjustment"
          RefundAdjustment        -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Refund Adjustment"
          AuthorizationAdjustment -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Authorization Adjustment"
    )
    adjs

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTransactions :: [(Transaction, UTCTime)] -> Html
renderTransactions transactions = do
  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
      "Quick Stats"
    renderQuickStats $ fmap fst transactions

  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Transactions"
    renderTransactionTable $ filter (\(Transaction {..}, _) -> getMonetaryValue trxDisplayAmount /= 0) transactions

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTransactionTable :: [(Transaction, UTCTime)] -> Html
renderTransactionTable transactions = do
  H.div ! A.class_ "flex flex-col" $ do
    H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
      H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
        H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
          H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Id"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Purchaser"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "State"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Description"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "MCC"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Rev"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Amount"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Purchase time"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Last Update at"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Merchant"
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  "Message"
            H.tbody $ do
              let firstCellClasses = "px-6 py-4 whitespace-no-wrap text-sm leading-5 font-medium text-gray-900"
                  cellClasses      = "px-6 py-4 whitespace-no-wrap text-sm leading-5 text-gray-500"
                  linkClasses      = "text-indigo-600 hover:text-indigo-900"
              let createRow (Transaction {..}, time) = do
                    H.tr ! A.class_ "bg-white" $ do
                      H.td ! A.class_ firstCellClasses $ do
                        H.a ! A.href (generateLinkToTransaction trxId) ! A.class_ linkClasses $ do
                          renderTransactionId trxId
                      H.td ! A.class_ cellClasses $ do
                        H.a ! A.href (generateLinkToUserPage trxUserId) ! A.class_ linkClasses $ do
                          renderUserID trxUserId
                      H.td ! A.class_ cellClasses $ do
                        renderTrxStateBubble trxState
                      H.td ! A.class_ cellClasses $ do
                        toHtml $ fromMaybe "" trxDescription
                      H.td ! A.class_ cellClasses $ do
                        case trxMerchant of
                          Nothing -> mempty
                          Just CardMerchant { cmiMcc = MastercardMCC mcc, ..} -> do
                            H.span ! A.title (toValue cmiMccDesc) ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ do
                              toHtml mcc
                      H.td ! A.class_ cellClasses $ do
                        toHtml trxRevision
                      H.td ! A.class_ cellClasses $ do
                        let currencyBadge = if getIsoCode trxDisplayAmount /= "USD"
                              then H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                                toHtml $ getIsoCode trxDisplayAmount
                              else mempty
                        if getMonetaryValue trxDisplayAmount >= 0
                          then do
                            H.div $ toHtml $ showCurr trxDisplayAmount
                            H.div currencyBadge
                          else do
                            H.span ! A.class_ "text-red-600" $ toHtml $ showCurr trxDisplayAmount
                            H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800 mr-1") $ "Credit Trx"
                            currencyBadge
                      H.td ! A.class_ cellClasses $ do
                        showDateTime trxPurchasedAt
                      H.td ! A.class_ cellClasses $ do
                        showDateTime time
                      H.td ! A.class_ cellClasses $ do
                        renderTrxMerchant trxMerchant
                      H.td ! A.class_ cellClasses $ do
                        renderMessageLink trxMsgSource
              mapM_ createRow transactions

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderQuickStats :: [Transaction] -> Html
renderQuickStats transactions = do
  let notFailed Transaction {..} = (trxState == TrxAuthorized || trxState == TrxPending || trxState == TrxCompleted) && trxSourceEvent == StateTransition
  let isDeclined Transaction {..} = case trxState of
        TrxDeclined _ -> True
        _             -> False
  let notFailedTrxs = filter notFailed transactions
  let declinedTrxs  = filter isDeclined transactions
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-4" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "All attempted count"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toHtml $ length transactions
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Declined count"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toHtml . length $ filter isDeclined transactions
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total completed count"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toHtml $ length notFailedTrxs
  -- row 2
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-4" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Est revenue"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let amount = foldr (\Transaction {..} accum -> getMonetaryValue trxDisplayAmount + accum) 0 notFailedTrxs
            let double :: Double  = fromRational $ amount * 0.009
            let normalized :: Int = ceiling double
            H.span "$"
            H.span $ toHtml normalized
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Avg amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let amount = foldr (\Transaction {..} accum -> getMonetaryValue trxDisplayAmount + accum) 0 notFailedTrxs
            let double :: Double = fromRational $ amount / toRational (length notFailedTrxs)
            let normalized :: Int = floor double
            H.span "$"
            H.span $ toHtml normalized
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Uniq user count"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let purchasers = nub $ fmap (\Transaction {..} -> trxUserId) transactions
            toHtml $ length purchasers
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Uniq group count"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let purchasers = nub $ foldr
                  (\Transaction {..} accum -> case trxGroupId of
                    Just (g, _) -> g : accum
                    Nothing     -> accum
                  )
                  []
                  transactions
            toHtml $ length purchasers
  -- row 3
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-4" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total processed amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let amount = foldr (\Transaction {..} accum -> abs (getMonetaryValue trxDisplayAmount) + accum) 0 transactions
            let double :: Double = fromRational amount
            H.span "$"
            H.span $ toHtml double
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total completed amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let amount = foldr (\Transaction {..} accum -> abs (getMonetaryValue trxDisplayAmount) + accum) 0 notFailedTrxs
            let double :: Double = fromRational amount
            H.span "$"
            H.span $ toHtml double
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total declined amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let amount = foldr (\Transaction {..} accum -> abs (getMonetaryValue trxDisplayAmount) + accum) 0 declinedTrxs
            let double :: Double = fromRational amount
            H.span "$"
            H.span $ toHtml double
