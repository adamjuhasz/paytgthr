{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.User where

import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , dataTable
                                                , indigoColorButton
                                                , mediumButton
                                                , redColorButton
                                                , tableCellClasses
                                                , tableFirstCellClasses
                                                , tableHeaderCellClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( renderMessageLink
                                                , showCurr
                                                , showDate
                                                , showDateTime
                                                , showDateWithoutTZ
                                                )
import           Chewpaca.Web.Groups            ( extGroupId
                                                , generateLinkToGroup
                                                , renderEndingDate
                                                , renderGroupId
                                                , renderGroupStateBubble
                                                , renderMembers
                                                , renderSplits
                                                , renderStartDate
                                                )
import           Chewpaca.Web.Ledger.Journal    ( renderAJournalList )
import           Chewpaca.Web.Payments          ( extPayId
                                                , generateLinkToPayment
                                                , renderPayTypeBubble
                                                , renderPaymentId
                                                , renderPaymentStateBubble
                                                )
import           Chewpaca.Web.Transactions      ( generateLinkToTransaction
                                                , renderTransactionId
                                                , renderTrxStateBubble
                                                )
import           Chewpaca.Web.Users             ( extUserID
                                                , generateLinkToUserPage
                                                , renderEmail
                                                , renderEmailValue
                                                , renderKYCtateBubble
                                                , renderUserID
                                                , renderUserStateBubble
                                                , valueUserID
                                                )
import           Chewpaca.Web.Utils             ( uuidToStrStart )
import           Control.Monad                  ( forM_
                                                , when
                                                )
import           Data.Functor                   ( (<&>) )
import           Data.List                      ( sort )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.UUID                      ( toText )
import           GHC.Exts                       ( coerce )
import           Shared.Models.Apto.Base        ( AptoCardholderId(..) )
import           Shared.Models.Card             ( AptoCardId(AptoCardId)
                                                , CardId(..)
                                                , CardModel(..)
                                                , IssuerPlatform(..)
                                                , PrivacyCardToken(..)
                                                )
import           Shared.Models.Currency         ( Currency(..)
                                                , getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.Group            ( GroupMember(..)
                                                , GroupModel(..)
                                                )
import           Shared.Models.Ids              ( JournalId(..)
                                                , LedgerEntryId(..)
                                                , LedgerTrxId(..)
                                                , UserID(..)
                                                )
import           Shared.Models.KYCAssesment     ( KYCAssesment(..)
                                                , KYCFailureReasons(..)
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal   ( LedgerJournal(..) )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentStatus(..)
                                                , PaymentSubType(..)
                                                , PaymentType(..)
                                                )
import           Shared.Models.PaymentAuth      ( PlaidBalanceRow(..) )
import           Shared.Models.Plaid.Base       ( Account(..) )
import           Shared.Models.RiskScore        ( RiskFact(..)
                                                , RiskScore(..)
                                                , riskAdjustedLimit
                                                )
import           Shared.Models.Token            ( TokenType(..)
                                                , UserToken(..)
                                                )
import           Shared.Models.Transaction      ( MastercardMCC(..)
                                                , MerchantInfo(..)
                                                , Transaction(..)
                                                , TransactionId(..)
                                                , TransactionState(..)
                                                )
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , RedactedText(..)
                                                , UserModel(..)
                                                )
import           Shared.TgthrMessages.Base      ( AccountType(Depository)
                                                , DepositoryType
                                                  ( Checking
                                                  , DepositoryTypeUnknown
                                                  , Prepaid
                                                  , Savings
                                                  )
                                                )
import           Shared.Transactions.UserGroupConsistencyCheck
                                                ( GroupCheckFailure(..)
                                                , GroupCheckStrictness(..)
                                                , UserCheckFailure(..)
                                                , checkGroupConsistency
                                                , checkUserConsistency
                                                )
import           System.IO.Unsafe               ( unsafePerformIO )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

getTimeOfTimeline :: Timeline -> UTCTime
getTimeOfTimeline (PurchaseMade t _) = t
getTimeOfTimeline (PaymentMade  t _) = t
getTimeOfTimeline (LedgerUpdate t _) = t

renderCardId :: CardId -> Html
renderCardId (CardId u) = case T.splitOn "-" $ toText u of
  []             -> "Error: Can't split cardid"
  (firstSec : _) -> toHtml (firstSec <> "...")

data Timeline
  = PurchaseMade UTCTime Transaction
  | PaymentMade UTCTime Payment
  | LedgerUpdate UTCTime LedgerEntry
  deriving (Eq)
instance Ord Timeline where
  compare a b = compare (getTimeOfTimeline a) (getTimeOfTimeline b)

data UserInfo = UserInfo
  { models         :: [(UserModel, UTCTime)]
  , spendable      :: Currency
  , liability      :: Currency
  , payments       :: [Payment]
  , transactions   :: [Transaction]
  , ledger         :: [(LedgerEntry, UTCTime)]
  , groups         :: [(GroupModel, UTCTime)]
  , riskScores     :: [RiskScore]
  , balances       :: [(PlaidBalanceRow, UTCTime)]
  , userDB         :: [(UserID, UserModel)]
  , tokens         :: [UserToken]
  , now            :: UTCTime
  , cards          :: [CardModel]
  , assessments    :: [KYCAssesment]
  , notes          :: Text
  , ledgerJournals :: [LedgerJournal]
  }

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderUser :: UserInfo -> Html
renderUser UserInfo { models = [] }                              = "Error: No models to load"
renderUser ui@UserInfo { models = models@(headOfModels : _), ..} = do
  let mostRecentModel = fst headOfModels
  let _userTimeline = sort $ fmap (\t -> PurchaseMade (trxPurchasedAt t) t) transactions <> fmap (\(le, t) -> LedgerUpdate t le) ledger
  let userId          = usrUserID mostRecentModel

  -- Scroll to top
  H.div ! A.id "top" $ mempty

  -- Recent card
  renderInfoCard userDB mostRecentModel (fmap fst groups)

  H.div $ do
    H.h3 ! A.id "notes" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Notes"
    renderNotes userId notes

  H.div $ do
    H.h3 ! A.id "kycassessments" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "KYC"
    renderAssessments userId assessments

  -- Quick Stats
  H.div $ do
    H.h3 ! A.id "quickstats" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Quick Stats"
    renderQuickStats ui

  H.div $ do
    H.h3 ! A.id "tokens" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Tokens"
    renderTokens now userId tokens

   -- ledger
  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Ledger Journals"
    renderJournalActions userId
    renderAJournalList ledgerJournals


  -- ledger
  H.div $ do
    H.h3 ! A.id "ledger" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Ledger"
    renderLedger userId ledgerJournals ledger

  -- risk scores
  H.div $ do
    H.h3 ! A.id "riskscores" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Risk Scores"
    renderRiskScores userId riskScores

    -- Cards
  H.div $ do
    H.h3 ! A.id "cards" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Cards"
    renderCards userId cards

  -- Transaction List
  H.div $ do
    H.h3 ! A.id "transactions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Transactions"
    renderTransactionList mostRecentModel transactions

  -- Payment list
  H.div $ do
    H.h3 ! A.id "payments" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Payments"
    renderPaymentList payments

  -- Groups
  H.div $ do
    H.h3 ! A.id "groups" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Groups"
    renderGroups userId groups

  H.div $ do
    H.h3 ! A.id "revisions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Revisions"
    renderUserRevisions models

  H.div $ do
    H.h3 ! A.id "balances" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "Plaid balances"
    renderBalances balances

  -- Actions
  H.div $ do
    H.h3 ! A.id "actions" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
      "User Actions"
    renderUserActions mostRecentModel

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderInfoCard :: [(UserID, UserModel)] -> UserModel -> [GroupModel] -> Html
renderInfoCard userDB mostRecentModel groups = do
  let (UserID userId) = usrUserID mostRecentModel
  let uid             = toValue (toText userId)
  let buttonClasses = "inline-flex items-center px-2.5 py-1.5 border border-gray-300 text-xs leading-4 font-medium rounded text-gray-700 bg-white hover:text-gray-500 focus:outline-none focus:border-blue-300 focus:shadow-outline-blue active:text-gray-800 active:bg-gray-50 transition ease-in-out duration-150"
  let cardLeftText  = "text-sm leading-5 font-medium text-gray-500"
      cardRightText = "mt-1 text-sm leading-5 text-gray-900 sm:mt-0 sm:col-span-2"
      cardDataRow   = "mt-8 sm:mt-0 sm:grid sm:grid-cols-3 sm:gap-4 sm:border-t sm:border-gray-200 sm:px-6 sm:py-5"
  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-lg" $ do
    H.div ! A.class_ "px-4 py-5 border-b border-gray-200 sm:px-6" $ do
      H.h3 ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
        toHtml $ fromMaybe "" (usrFirstName mostRecentModel) <> " " <> fromMaybe "" (usrLastName mostRecentModel)
      H.p ! A.class_ "mt-1 max-w-2xl text-sm leading-5 text-gray-500" $ do
        H.span ! A.class_ "mr-2" $ do
          renderEmail (usrEmail mostRecentModel)
        H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
          let jscript = "navigator.clipboard.writeText('" <> renderEmailValue (usrEmail mostRecentModel) <> "')"
          H.button ! A.type_ "button" ! A.onclick jscript ! A.class_ buttonClasses $ do
            "Copy Email"
    H.div ! A.class_ "px-4 py-5 sm:p-0" $ do
      H.dl $ do
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Statuses"
          H.dd ! A.class_ cardRightText $ do
            renderUserStateBubble $ usrUserState mostRecentModel
            H.br
            renderKYCtateBubble $ usrAptoKYCStatus mostRecentModel
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Id"
          H.dd ! A.class_ cardRightText $ do
            H.span $ do
              toHtml . extUserID $ usrUserID mostRecentModel
            H.span $ do
              let jscript = "navigator.clipboard.writeText('" <> valueUserID (usrUserID mostRecentModel) <> "')"
              H.button ! A.type_ "button" ! A.onclick jscript ! A.class_ buttonClasses $ do
                "Copy ID"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "App Events Feed"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href ("/appevents/user/" <> valueUserID (usrUserID mostRecentModel)) ! A.class_ tableLinkClasses $ "View App Events"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Messages Sent"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href ("https://fly.customer.io/env/89189/people/" <> valueUserID (usrUserID mostRecentModel) <> "/sent") ! A.class_ tableLinkClasses $ "Customer.io"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Sync to Customer.io"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href ("/user/" <> valueUserID (usrUserID mostRecentModel) <> "/sync/segment") ! A.class_ tableLinkClasses $ "Sync now"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Dwolla Payment History"
          H.dd ! A.class_ cardRightText $ do
            let dashboardUrl = usrDwollaId mostRecentModel <&> T.replace "api.dwolla.com" "dashboard.dwolla.com" <&> (<> "/transactions")
            case dashboardUrl of
              Nothing  -> mempty
              Just url -> do
                H.a ! A.href (toValue url) ! A.class_ tableLinkClasses $ "Dwolla dashboard"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Zendesk History"
          H.dd ! A.class_ cardRightText $ do
            H.a ! A.href ("https://paytgthrhelp.zendesk.com/agent/search/1?type=ticket&q=" <> renderEmailValue (usrEmail mostRecentModel)) ! A.class_ tableLinkClasses $ do
              "Zendesk email search"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Phone"
          H.dd ! A.class_ cardRightText $ do
            case usrPhone mostRecentModel of
              Nothing              -> mempty
              Just (PhoneNumber t) -> do
                let areaCode = T.take 3 t
                H.span $ toHtml areaCode
                H.span "-"
                H.span . toHtml . T.drop 3 $ T.take 6 t
                H.span "-"
                H.span . toHtml $ T.drop 6 t
                H.br
                H.span $ H.a ! A.href ("/users/search/text?q=" <> toValue t) ! A.class_ tableLinkClasses $ "Find users with same #"
                H.br
                H.span $ H.a ! A.href ("https://www.allareacodes.com/" <> toValue areaCode) ! A.class_ tableLinkClasses $ "Explore area code"

        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Phone Verified"
          H.dd ! A.class_ cardRightText $ do
            if usrPhoneVerified mostRecentModel then "Phone Verifed" else "Not Verified"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Email Verified"
          H.dd ! A.class_ cardRightText $ do
            if usrEmailVerified mostRecentModel then "Email Verifed" else "Not Verified"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Apto Cardholder Id"
          H.dd ! A.class_ cardRightText $ do
            let normalizeCID Nothing                     = ""
                normalizeCID (Just (AptoCardholderId t)) = t
            let normalized = normalizeCID (usrAptoCardholderID mostRecentModel)
            H.span ! A.class_ "mr-2" $ do
              toHtml normalized
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              let jscript = "navigator.clipboard.writeText('" <> toValue normalized <> "')"
              H.button ! A.type_ "button" ! A.onclick jscript ! A.class_ buttonClasses $ do
                "Copy"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Privacy Account Token"
          H.dd ! A.class_ cardRightText $ do
            case usrPrivacyAcctToken mostRecentModel of
              Nothing -> do
                let url = "/user/" <> uid <> "/privacy/sendto"
                H.form ! A.action url ! A.method "post" $ do
                  H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                  H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                    "Create account at Privacy"
              Just t -> do
                H.span ! A.class_ "mr-2" $ do
                  toHtml t
                H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
                  let jscript = "navigator.clipboard.writeText('" <> toValue t <> "')"
                  H.button ! A.type_ "button" ! A.onclick jscript ! A.class_ buttonClasses $ do
                    "Copy"

        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Address"
          H.dd ! A.class_ cardRightText $ do
            let fullAddress = T.replace "#" "%23" . T.replace " " "+" $ fromMaybe "" (usrAddressStreet mostRecentModel) <> " " <> fromMaybe "" (usrAddressCity mostRecentModel) <> " " <> fromMaybe "" (usrAddressState mostRecentModel) <> " " <> fromMaybe "" (usrAddressZip mostRecentModel)
            let link = toValue $ "https://www.google.com/maps/search/?api=1&query=" <> fullAddress
            let street      = fromMaybe "" (usrAddressStreet mostRecentModel)
            let street2     = fromMaybe "" (usrAddressStreet2 mostRecentModel)
            let city        = fromMaybe "" (usrAddressCity mostRecentModel)
            let state       = fromMaybe "" (usrAddressState mostRecentModel)
            let zipcode     = fromMaybe "" (usrAddressZip mostRecentModel)
            let bottomLine  = city <> ", " <> state <> " " <> zipcode
            H.a ! A.href link ! A.class_ tableLinkClasses $ do
              H.div $ do
                toHtml street
              when
                (isJust $ usrAddressStreet2 mostRecentModel)
                (do
                  H.div $ do
                    toHtml street2
                )
              H.div $ do
                H.span $ toHtml bottomLine
            H.div $ do
              H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
                let jscript = toValue $ "navigator.clipboard.writeText('" <> street <> "\\n" <> street2 <> "\\n" <> bottomLine <> "')"
                H.button ! A.type_ "button" ! A.onclick jscript ! A.class_ buttonClasses $ do
                  "Copy"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Bank verified"
          H.dd ! A.class_ cardRightText $ do
            case usrBankVerified mostRecentModel of
              Just True -> H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ "Verified"
              _         -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Unverified"
            H.span " "
            case usrBankVerifedAmounts mostRecentModel of
              Nothing      -> mempty
              Just amounts -> mapM_
                (\a -> do
                  H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ toHtml a
                  H.span " "
                )
                amounts
            H.span " "
            case usrBankType mostRecentModel of
              Nothing -> mempty
              Just (Depository Checking) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Checking"
              Just (Depository Savings) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Savings"
              Just (Depository Prepaid) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Prepaid"
              Just (Depository DepositoryTypeUnknown) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Unknown"
              Just _  -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Other"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Bank Name"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ fromMaybe "" (usrBankName mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Bank Account Name"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ fromMaybe "" (usrBankAccountName mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Dwolla Id"
          H.dd ! A.class_ cardRightText $ do
            case usrDwollaId mostRecentModel of
              Nothing -> do
                let url = "/user/" <> uid <> "/dwolla/sendto"
                H.form ! A.action url ! A.method "post" $ do
                  H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                    "Create account at Dwolla"
              Just dwollaID -> toHtml dwollaID
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Dwolla Funding Id"
          H.dd ! A.class_ cardRightText $ do
            toHtml $ fromMaybe "" (usrDwollaFundingId mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Date of birth"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateWithoutTZ (usrDOB mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "SSN"
          H.dd ! A.class_ cardRightText $ do
            case usrSSN mostRecentModel of
              Nothing                   -> mempty
              Just (RedactedText enced) -> do
                H.a ! A.href ("/user/" <> uid <> "/ssn") ! A.class_ (mediumButton <> indigoColorButton <> " mr-2") $ do
                  "View"
                H.span $ do
                  toHtml enced
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Created on"
          H.dd ! A.class_ cardRightText $ do
            showDateTime $ usrCreatedOn mostRecentModel
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "First signin on"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateTime (usrFirstSignIn mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Disclose accepted on"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateTime (usrDislcosureOk mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Consent accepted on"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateTime (usrConstentOk mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Became active on"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateTime (usrBecameActiveOn mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Card created on"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateTime (usrCardCreatedOn mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Card activated on"
          H.dd ! A.class_ cardRightText $ do
            foldMap showDateTime (usrCardActivatedOn mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Self Transaction Status (does not check partner)"
          H.dd ! A.class_ cardRightText $ do
            H.div $ case checkUserConsistency mostRecentModel of
              Right _ -> H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ do
                "User can make purchases"
              Left _ -> H.div $ do
                H.span ! A.class_ (badgeClasses <> " mb-1 bg-red-100 text-red-800") $ do
                  "User can't make purchases"
            H.div $ case checkUserConsistency mostRecentModel of
              Right _               -> mempty
              Left  UserIsNotActive -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "UserIsNotActive"
              Left UserIsClosed -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "UserIsClosed"
              Left NoFundingSourceLinked -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "NoFundingSourceLinked"
              Left NoFundingSourceVerified -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "NoFundingSourceVerified"
              Left KYCNotPassed -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "KYCNotPassed"
            H.div $ do
              H.span ! A.class_ (badgeClasses <> "mt-1 mr-1 bg-gray-100 text-gray-800") $ do
                "Revision " <> toHtml (usrRevision mostRecentModel)
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Group Transaction Status (does not check self or partner users)"
          H.dd ! A.class_ cardRightText $ do
            let now = unsafePerformIO getCurrentTime
            case checkGroupConsistency StrictGroupCheck now groups of
              Right GroupModel {..} -> do
                H.div $ do
                  H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ do
                    "Group can make purchases"
                H.div $ do
                  H.a ! A.href (generateLinkToGroup grpId) $ do
                    H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800") $ do
                      renderGroupId grpId
                  H.span ! A.class_ (badgeClasses <> "ml-1 bg-gray-100 text-gray-800") $ do
                    "Revision " <> toHtml grpRevision
              Left NoActiveGroup -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "No active group"
              Left MultiplePermGroups -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                "Multiple permanaent groups"
        H.div ! A.class_ cardDataRow $ do
          H.dt ! A.class_ cardLeftText $ do
            "Partner Transaction Status (does not check self)"
          H.dd ! A.class_ cardRightText $ do
            case groups of
              [] -> H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ do
                "No group"
              (theGroup : _) -> do
                let partnerList = filter (\GroupMember {..} -> mbrUser /= usrUserID mostRecentModel) . grpMembers $ theGroup
                case partnerList of
                  [] -> do
                    H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                      "ERROR: No partner in group"
                  (GroupMember { mbrUser = partner } : _) -> do
                    let parnterModelMaybe = lookup partner userDB
                    case parnterModelMaybe of
                      Nothing -> do
                        H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                          "ERROR: Partner not in userDB"
                      Just parnterModel -> do
                        let partnerId          = usrUserID parnterModel
                        let partnerRev         = usrRevision parnterModel
                        let partnerConsistency = checkUserConsistency parnterModel
                        H.div $ case partnerConsistency of
                          Right _ -> H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ do
                            "Partner can make purchases"
                          Left _ -> H.span ! A.class_ (badgeClasses <> "mb-1 bg-red-100 text-red-800") $ do
                            "Partner can't make purchases"
                        H.div $ case partnerConsistency of
                          Right _               -> mempty
                          Left  UserIsNotActive -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                            "UserIsNotActive"
                          Left UserIsClosed -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                            "UserIsClosed"
                          Left NoFundingSourceLinked -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                            "NoFundingSourceLinked"
                          Left NoFundingSourceVerified -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                            "NoFundingSourceVerified"
                          Left KYCNotPassed -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                            "KYCNotPassed"
                        H.div $ case partnerConsistency of
                          Right _ -> mempty
                          Left  _ -> do
                            H.a ! A.href (generateLinkToUserPage partnerId) $ do
                              H.span ! A.class_ (badgeClasses <> "mt-1 mr-1 bg-purple-100 text-purple-800") $ do
                                renderUserID partnerId
                            H.span ! A.class_ (badgeClasses <> "mt-1 mr-1 bg-gray-100 text-gray-800") $ do
                              "Revision " <> toHtml partnerRev


-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderQuickStats :: UserInfo -> Html
renderQuickStats UserInfo {..} = do
  let mostRecentRiskMaybe = listToMaybe riskScores
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-3" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Personal Limit"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            case mostRecentRiskMaybe of
              Nothing             -> mempty
              Just mostRecentRisk -> do
                let limit = riskAdjustedLimit mostRecentRisk
                toHtml $ showCurr limit
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Spenable Balance"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toHtml $ showCurr spendable
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Liability"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toHtml $ showCurr liability
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-3" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total completed trxs amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let trxSum = foldr
                  (\Transaction {..} accum -> case (trxState, trxDisplayAmount) of
                    (TrxCompleted, Currency "USD" _) -> accum + trxDisplayAmount
                    (_           , _               ) -> accum
                  )
                  (Currency "USD" 0)
                  transactions
            toHtml $ showCurr trxSum
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Pending payments total"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            let pendingPayments = filter (\Payment {..} -> payStatus == PaymentPending) payments
            let total = foldr
                  (\Payment {..} accum -> case payType of
                    DebitFromUser -> accum + payAmount
                    CreditToUser  -> accum - payAmount
                  )
                  (Currency "USD" 0)
                  pendingPayments
            toHtml $ showCurr total

extTrxId :: TransactionId -> Text
extTrxId (TransactionId t) = toText t

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTransactionList :: UserModel -> [Transaction] -> Html
renderTransactionList mostRecentModel transactions = do
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Id"
        H.th ! A.class_ tableHeaderCellClasses $ "Purchaser"
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Merchant"
        H.th ! A.class_ tableHeaderCellClasses $ "Description"
        H.th ! A.class_ tableHeaderCellClasses $ "Amount"
        H.th ! A.class_ tableHeaderCellClasses $ "Purchase time"
        H.th ! A.class_ tableHeaderCellClasses $ "Split"
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
    H.tbody $ do
      let createRow Transaction {..} = do
            H.tr ! A.id (toValue $ "trx-" <> extTrxId trxId) ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                H.a ! A.href (generateLinkToTransaction trxId) ! A.class_ tableLinkClasses $ do
                  renderTransactionId trxId
              H.td ! A.class_ tableCellClasses $ do
                if trxUserId == usrUserID mostRecentModel
                  then do
                    "Self"
                  else do
                    H.a ! A.href (generateLinkToUserPage trxUserId) ! A.class_ tableLinkClasses $ do
                      renderUserID trxUserId
              H.td ! A.class_ tableCellClasses $ do
                renderTrxStateBubble trxState
              H.td ! A.class_ tableCellClasses $ do
                case trxMerchant of
                  Nothing -> mempty
                  Just CardMerchant { cmiMcc = MastercardMCC mcc, cmiMccDesc = mccDesc } -> do
                    H.span ! A.title (toValue mccDesc) ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ do
                      toHtml mcc
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ fromMaybe "" trxDescription
              H.td ! A.class_ tableCellClasses $ do
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
              H.td ! A.class_ tableCellClasses $ do
                showDateTime trxPurchasedAt
              H.td ! A.class_ tableCellClasses $ do
                let printSelf (_, amount) = do
                      let double :: Double = fromRational amount
                      H.span $ toHtml double
                      H.span "% "
                      H.span "Self"
                let printPartner (user, amount) = do
                      let double :: Double = fromRational amount
                      H.span $ toHtml double
                      H.span "% "
                      H.a ! A.href (generateLinkToUserPage user) ! A.class_ tableLinkClasses $ do
                        renderUserID user
                mapM_
                  (\(user, amount) -> do
                    H.div $ do
                      if user == usrUserID mostRecentModel then printSelf (user, amount) else printPartner (user, amount)
                  )
                  trxSplitAmounts
              H.td ! A.class_ tableCellClasses $ do
                toHtml trxRevision

      mapM_ createRow transactions

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderPaymentList :: [Payment] -> Html
renderPaymentList payments = do

  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Id"
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
        H.th ! A.class_ tableHeaderCellClasses $ "Initiated"
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Amount"
        H.th ! A.class_ tableHeaderCellClasses $ "Direction"
        H.th ! A.class_ tableHeaderCellClasses $ "Action"
    H.tbody $ do
      let createRow Payment {..} = do
            H.tr ! A.id (toValue $ "payment-" <> extPayId payId) ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                H.a ! A.href (generateLinkToPayment payId) ! A.class_ tableLinkClasses $ do
                  renderPaymentId payId
              H.td ! A.class_ tableCellClasses $ do
                toHtml payRevision
              H.td ! A.class_ tableCellClasses $ do
                showDateTime payCreatedAt
              H.td ! A.class_ tableCellClasses $ do
                H.div $ do
                  renderPaymentStateBubble payStatus
                case paySubType of
                  InitialVerification -> H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800 mr-1") $ "Hidden"
                  RefundVerification  -> H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800 mr-1") $ "Hidden"
                  NormalPayment       -> mempty
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ showCurr payAmount
              H.td ! A.class_ tableCellClasses $ do
                renderPayTypeBubble payType
              H.td ! A.class_ tableCellClasses $ do
                let url = "/send/cancelpayment"
                case payStatus of
                  PaymentCreated -> do
                    H.form ! A.action url ! A.method "post" $ do
                      H.input ! A.type_ "hidden" ! A.name "payid" ! A.value (toValue $ extPayId payId)
                      H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                        "Cancel"
                  PaymentPending -> do
                    H.form ! A.action url ! A.method "post" $ do
                      H.input ! A.type_ "hidden" ! A.name "payid" ! A.value (toValue $ extPayId payId)
                      H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                        "Cancel"
                  _ -> mempty

      mapM_ createRow payments

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderCards :: UserID -> [CardModel] -> Html
renderCards (UserID u) cards = do
  let uid = toValue $ toText u

  let createRow CardModel { cardId = CardId card, ..} = do
        H.tr ! A.class_ "bg-white" $ do
          H.td ! A.class_ tableFirstCellClasses $ do
            renderCardId $ CardId card
          H.td ! A.class_ tableCellClasses $ do
            toHtml $ case cardPlatform of
              AptoPayments   (AptoCardId       t) -> "Apto (" <> t <> ")"
              PayWithPrivacy (PrivacyCardToken t) -> "Privacy (" <> head (T.splitOn "-" t) <> ")"
          H.td ! A.class_ tableCellClasses $ do
            toHtml $ show cardRevision
          H.td ! A.class_ tableCellClasses $ do
            toHtml $ show cardLastFour
          H.td ! A.class_ tableCellClasses $ do
            toHtml $ show cardDesign
          H.td ! A.class_ tableCellClasses $ do
            toHtml $ show cardStatus
          H.td ! A.class_ tableCellClasses $ do
            toHtml $ show cardMemo
          H.td ! A.class_ tableCellClasses $ do
            showDateTime createdAt
          H.td ! A.class_ tableCellClasses $ do
            maybe "" showDateTime activatedAt
          H.td ! A.class_ tableCellClasses $ do
            maybe "" showDateTime closedAt
          H.td ! A.class_ tableCellClasses $ do
            showDateTime updatedAt
          H.td ! A.class_ tableCellClasses $ do
            let cid = toValue $ toText card
            H.a ! A.href ("/user/" <> uid <> "/card/" <> cid <> "/pan") ! A.class_ (mediumButton <> indigoColorButton <> " mr-1") $ do
              "PAN"
            H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/card/" <> cid <> "/state/lock") ! A.class_ "inline mr-1" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Lock"
            H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/card/" <> cid <> "/state/adminlock") ! A.class_ "inline mr-1" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Admin Lock"
            H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/card/" <> cid <> "/state/unlock") ! A.class_ "inline mr-1" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Unlock"
            H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/card/" <> cid <> "/state/close") ! A.class_ "inline" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                "Close"

  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-md mb-4" $ do
    H.ul $ do
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Issue a card "
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let urlDigital = "/user/" <> uid <> "/card/create/DigitalWallet"
              H.form ! A.action urlDigital ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Issue a new Digital Wallet card"
              let urlBlack = "/user/" <> uid <> "/card/create/PhysicalBlack"
              H.form ! A.action urlBlack ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Issue a new Black Physical card"

  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Id"
        H.th ! A.class_ tableHeaderCellClasses $ "Issuer ID"
        H.th ! A.class_ tableHeaderCellClasses $ "Revision"
        H.th ! A.class_ tableHeaderCellClasses $ "Last Four"
        H.th ! A.class_ tableHeaderCellClasses $ "Design"
        H.th ! A.class_ tableHeaderCellClasses $ "Status"
        H.th ! A.class_ tableHeaderCellClasses $ "Memo"
        H.th ! A.class_ tableHeaderCellClasses $ "created at"
        H.th ! A.class_ tableHeaderCellClasses $ "activated at"
        H.th ! A.class_ tableHeaderCellClasses $ "closed at"
        H.th ! A.class_ tableHeaderCellClasses $ "updated at"
        H.th ! A.class_ tableHeaderCellClasses $ "Actions"
    H.tbody $ do
      mapM_ createRow cards

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderLedger :: UserID -> [LedgerJournal] -> [(LedgerEntry, UTCTime)] -> Html
renderLedger (UserID u) journals ledger = do
  let uid = toValue $ toText u

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/ledger/adjustment") $ do
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "ledgerchange" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Manual Adjustment"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "amount" ! A.id "ledgerchange" ! A.placeholder "1.12" ! A.type_ "decimal" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          H.p ! A.class_ "mt-2 text-sm text-gray-500" $ do
            "without dollar sign, 2 decimal places, +/- ex: (1.12) (-13.45)"

          H.label ! A.for "from" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "From Journal"
          H.select ! A.name "from" ! A.id "from" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5" $ do
            H.option "Pick one of these as FROM"
            H.option ! A.value "00000000-0000-0000-0000-000000000000" $ "Purchase disputes (Lithic) (6c101d7a)"
            H.option ! A.value "00000000-0000-0000-0000-000000000000" $ "Customer Research Rewards (82620720)"
            H.option ! A.value "00000000-0000-0000-0000-000000000000" $ "Account Growth Rewards (bc1ef6e9)"
            H.option ! A.value "00000000-0000-0000-0000-000000000000" $ "Engineering Mistakes (f6bf2fc9)"
            H.option ! A.value "00000000-0000-0000-0000-000000000000" $ "Accepted Losses (97f8186c)"

            forM_ journals $ \LedgerJournal {..} -> do
              H.option ! A.value (toValue . toText $ coerce journalId) $ toHtml $ journalName <> " (" <> uuidToStrStart journalId <> ")"

          H.label ! A.for "to" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "To Journal"
          H.select ! A.name "to" ! A.id "to" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5" $ do
            H.option "Pick one of these as TO"
            forM_ journals $ \LedgerJournal {..} -> do
              H.option ! A.value (toValue . toText $ coerce journalId) $ toHtml $ journalName <> " (" <> uuidToStrStart journalId <> ")"

          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Ledger"

  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Id"
        H.th ! A.class_ tableHeaderCellClasses $ "Date"
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
        H.th ! A.class_ tableHeaderCellClasses $ "Balance"
        H.th ! A.class_ tableHeaderCellClasses $ "Pending Bal"
        H.th ! A.class_ tableHeaderCellClasses $ "Change"
        H.th ! A.class_ tableHeaderCellClasses $ "Type"
        H.th ! A.class_ tableHeaderCellClasses $ "Cause"
        H.th ! A.class_ tableHeaderCellClasses $ "ldg trx"
        H.th ! A.class_ tableHeaderCellClasses $ "Journal"
    H.tbody $ do
      let extLedId (LedgerEntryId l) = toText l
      let createRow (LedgerEntry {..}, time) = do
            H.tr ! A.id (toValue $ "ledger-" <> extLedId lenId) ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                toHtml $ uuidToStrStart lenId

              H.td ! A.class_ tableCellClasses $ do
                showDateTime time

              H.td ! A.class_ tableCellClasses $ do
                toHtml lenRevision

              H.td ! A.class_ tableCellClasses $ do
                toHtml $ showCurr lenBalance

              H.td ! A.class_ tableCellClasses $ do
                toHtml $ showCurr lenPendingBalance

              H.td ! A.class_ tableCellClasses $ do
                let change = case lenFact of
                      TrxAdjustment  _ a -> a
                      PaymentCleared _ a -> a
                      Manual         a   -> a
                      InitialBalance a   -> a
                      UserTransfer   a   -> a
                toHtml $ showCurr change
              H.td ! A.class_ tableCellClasses $ do
                case lenFact of
                  TrxAdjustment _ _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-purple-100 text-purple-800") $ "Adjustment"
                  PaymentCleared _ _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Payment"
                  Manual _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") ! A.title (toValue $ fromMaybe "" lenIdempotency) $ "Manual"
                  InitialBalance _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") ! A.title (toValue $ fromMaybe "" lenIdempotency) $ "Initial Balance"
                  UserTransfer _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") ! A.title (toValue $ fromMaybe "" lenIdempotency) $ "UserTransfer"

              H.td ! A.class_ tableCellClasses $ do
                case lenFact of
                  TrxAdjustment trxId _ -> do
                    H.a ! A.href (toValue $ "#trx-" <> extTrxId trxId) ! A.class_ tableLinkClasses $ do
                      H.span "Trx "
                      H.span $ renderTransactionId trxId
                  PaymentCleared payId _ -> do
                    H.a ! A.href (toValue $ "#payment-" <> extPayId payId) ! A.class_ tableLinkClasses $ do
                      H.span "Payment "
                      H.span $ renderPaymentId payId
                  Manual _ -> do
                    mempty
                  InitialBalance _ -> do
                    mempty
                  UserTransfer _ -> do
                    mempty

              H.td ! A.class_ tableCellClasses $ do
                case lenTransaction of
                  Nothing                -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "No Trx"
                  Just (LedgerTrxId lid) -> toHtml $ uuidToStrStart lid

              H.td ! A.class_ tableCellClasses $ do
                toHtml $ uuidToStrStart lenJournal

      mapM_ createRow ledger

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderUserActions :: UserModel -> Html
renderUserActions UserModel { usrUserID = UserID u, ..} = do
  let uid = toValue $ toText u

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/email") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "newemail" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ "New Email"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "email" ! A.id "newemail" ! A.placeholder "email@example.com" ! A.type_ "email" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Email"

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/address") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.p "None of these are validated, double check before submiting!"
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          -- First line
          H.label ! A.for "firstLine" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Street"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "firstline" ! A.id "firstline" ! A.placeholder "123 Main Street Apt 12" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          -- First line
          H.label ! A.for "secondline" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Street 2 (Prefered on line 1)"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "secondline" ! A.id "secondline" ! A.placeholder "" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          -- City         
          H.label ! A.for "city" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "City"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            let cityPlaceholder = maybe "" toValue usrAddressCity
            H.input ! A.name "city" ! A.id "city" ! A.placeholder cityPlaceholder ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          -- State
          H.label ! A.for "state" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "State"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            let statePlaceholder = maybe "" toValue usrAddressState
            H.input ! A.name "state" ! A.id "state" ! A.placeholder statePlaceholder ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          -- Zip
          H.label ! A.for "zip" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Zip"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            let zipPlaceholder = maybe "" toValue usrAddressZip
            H.input ! A.name "zip" ! A.id "zip" ! A.placeholder zipPlaceholder ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          --Submit
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Address"

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/phone") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.p "This is not validated, double check before submiting!"
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          -- First line
          H.label ! A.for "phone" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Phone"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "phone" ! A.id "phone" ! A.placeholder "1231231234" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          --Submit
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Phone"

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/ssn") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "newssn" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "New SSN"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "ssn" ! A.id "newssn" ! A.placeholder "987-23-123" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change SSN"

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/name") $ do
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "newfname" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "First Name (Required)"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "fname" ! A.id "newfname" ! A.placeholder "Bill" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.label ! A.for "newlname" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Last Name (Required)"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "lname" ! A.id "newlname" ! A.placeholder "Gates" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Name"

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/dwolla") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "newdwollaid" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Dwolla ID (Can be empty)"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "dwollaid" ! A.id "newdwollaid" ! A.placeholder "" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.label ! A.for "newdwollafsid" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Funding ID  (Can be empty)"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "dwollafsid" ! A.id "newdwollafsid" ! A.placeholder "" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                "Change Dwolla IDs"

  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/change/fundingsource/bank") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.p "This is not validated, double check before submiting!"
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "bankname" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Bank Name"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "bankname" ! A.id "bankname" ! A.placeholder "Wells Fargo" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.label ! A.for "accountname" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Account Name"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "accountname" ! A.id "accountname" ! A.placeholder "Best Checking" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.label ! A.for "routingnumber" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Routing Number"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "routingnumber" ! A.id "routingnumber" ! A.placeholder "123456789" ! A.type_ "number" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          H.label ! A.for "accountnumber" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Account Number"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "accountnumber" ! A.id "accountnumber" ! A.placeholder "98765323" ! A.type_ "number" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

          --Submit
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Bank Details"

  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-md" $ do
    H.ul $ do
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Send statement to user"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/commands/sendstatement"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Send Statement"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Make a payment for the current ledger value minus pending payments"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/payment/create/ledgerbalance"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Manual payment for ledger"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Resend Verification Payments"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/fundingsource/verification/resend"
              H.form ! A.action url ! A.method "post" $ do
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Resend Verification"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Force verification of funding source"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/fundingsource/verify"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Manually verify FS"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Removing the current funding source"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/fundingsource/remove"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Remove FS"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Closes user due to request from said user"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/state/close/requested"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Close user as requested"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Closes user due overdue balance and too many failed payments (R01)"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/state/close/overdue"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Close becuase overdue balance"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Closes user due being a duplicate of another user"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/state/close/duplicate"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Close user as duplicate"

      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Closes user due fraudy behavior"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/state/close/fraudy"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Close user as fraudy"
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "Activate User"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> uid <> "/state/activate"
              H.form ! A.action url ! A.method "post" $ do
                H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Activate user"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderRiskScores :: UserID -> [RiskScore] -> Html
renderRiskScores (UserID u) scores = do
  let uid = toValue $ toText u
  H.div $ do
    H.form ! A.method "post" ! A.action ("/user/" <> uid <> "/risk/adjustment") $ do
      H.input ! A.type_ "hidden" ! A.name "userid" ! A.value uid
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.label ! A.for "riskadjustment" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
            "Manual Adjustment to (0 - 100)"
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "trustscore" ! A.id "riskadjustment" ! A.placeholder "40" ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"
          H.div ! A.class_ "mt-2" $ do
            H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Change Score"

  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Date"
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
        H.th ! A.class_ tableHeaderCellClasses $ "Score"
        H.th ! A.class_ tableHeaderCellClasses $ "Limit"
        H.th ! A.class_ tableHeaderCellClasses $ "Change"
        H.th ! A.class_ tableHeaderCellClasses $ "Fact"
        H.th ! A.class_ tableHeaderCellClasses $ "Link"
    H.tbody $ do
      let createRow rsk@RiskScore {..} = do
            H.tr ! A.id ("risk-" <> toValue rskRev) ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                showDateTime rskCreatedAt
              H.td ! A.class_ tableCellClasses $ do
                toHtml rskRev
              H.td ! A.class_ tableCellClasses $ do
                toHtml rskTrustScore
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ showCurr $ riskAdjustedLimit rsk
              H.td ! A.class_ tableCellClasses $ do
                toHtml rskChange
              H.td ! A.class_ tableCellClasses $ do
                case rskFact of
                  InitialRisk -> do
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Initial Risk"
                  ManualRiskAdj d -> do
                    H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ do
                      "Manually adjusted"
                    H.span " "
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ do
                      toHtml d
                  FailedPayment _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Failed payment"
                  SuccessfulPayment _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ "Completed payment"
                  ChangedLinkedAcct -> do
                    H.span ! A.class_ (badgeClasses <> " bg-orange-100 text-orange-800") $ "Changed linked account"
                  RiskyAcctBalance bal -> do
                    H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ do
                      "Risky balance"
                    H.span " "
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ do
                      toHtml (showCurr bal)
                  RiskyTransaction _ -> do
                    H.span ! A.class_ (badgeClasses <> " bg-yellow-100 text-yellow-800") $ "Risky Transaction"
              H.td ! A.class_ tableCellClasses $ do
                case rskFact of
                  InitialRisk         -> mempty
                  ManualRiskAdj _     -> mempty
                  FailedPayment payId -> do
                    H.a ! A.href (generateLinkToPayment payId) ! A.class_ tableLinkClasses $ do
                      H.span $ renderPaymentId payId
                  SuccessfulPayment payId -> do
                    H.a ! A.href (generateLinkToPayment payId) ! A.class_ tableLinkClasses $ do
                      H.span $ renderPaymentId payId
                  ChangedLinkedAcct      -> mempty
                  RiskyAcctBalance _     -> mempty
                  RiskyTransaction trxId -> do
                    H.a ! A.href (generateLinkToTransaction trxId) ! A.class_ tableLinkClasses $ do
                      H.span $ renderTransactionId trxId
      mapM_ createRow scores

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGroups :: UserID -> [(GroupModel, UTCTime)] -> Html
renderGroups self groups = do
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Last update"
        H.th ! A.class_ tableHeaderCellClasses $ "Id"
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
        H.th ! A.class_ tableHeaderCellClasses $ "Start"
        H.th ! A.class_ tableHeaderCellClasses $ "End"
        H.th ! A.class_ tableHeaderCellClasses $ "Members"
        H.th ! A.class_ tableHeaderCellClasses $ "Split"
    H.tbody $ do
      let createRow (GroupModel {..}, time) = do
            H.tr ! A.id (toValue $ "group-" <> extGroupId grpId) ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                showDate time
              H.td ! A.class_ tableCellClasses $ do
                H.a ! A.href (generateLinkToGroup grpId) ! A.class_ tableLinkClasses $ do
                  renderGroupId grpId
              H.td ! A.class_ tableCellClasses $ do
                renderGroupStateBubble grpStatus
              H.td ! A.class_ tableCellClasses $ do
                toHtml grpRevision
              H.td ! A.class_ tableCellClasses $ do
                renderStartDate grpStart
              H.td ! A.class_ tableCellClasses $ do
                renderEndingDate grpEnd
              H.td ! A.class_ tableCellClasses $ do
                mapM_ (renderMembers self) grpMembers
              H.td ! A.class_ tableCellClasses $ do
                mapM_ (renderSplits self) grpSplit

      mapM_ createRow groups

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderUserRevisions :: [(UserModel, UTCTime)] -> Html
renderUserRevisions revs = do
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Rev"
        H.th ! A.class_ tableHeaderCellClasses $ "Last Update"
        H.th ! A.class_ tableHeaderCellClasses $ "Account Status"
        H.th ! A.class_ tableHeaderCellClasses $ "Bank Verified"
        H.th ! A.class_ tableHeaderCellClasses $ "Name"
        H.th ! A.class_ tableHeaderCellClasses $ "Bank #s"
        H.th ! A.class_ tableHeaderCellClasses $ "Bank Names"
        H.th ! A.class_ tableHeaderCellClasses $ "Bank Type"
        H.th ! A.class_ tableHeaderCellClasses $ "Phone"
        H.th ! A.class_ tableHeaderCellClasses $ "DOB"
        H.th ! A.class_ tableHeaderCellClasses $ "SSN"
        H.th ! A.class_ tableHeaderCellClasses $ "Disclosure"
        H.th ! A.class_ tableHeaderCellClasses $ "Consent"
        H.th ! A.class_ tableHeaderCellClasses $ "Address"
        H.th ! A.class_ tableHeaderCellClasses $ "Dwolla Links"
        H.th ! A.class_ tableHeaderCellClasses $ "Email"
        H.th ! A.class_ tableHeaderCellClasses $ "Message"
    H.tbody $ do
      let createRow (UserModel {..}, time) = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                toHtml usrRevision
              H.td ! A.class_ tableCellClasses $ do
                showDateTime time
              H.td ! A.class_ tableCellClasses $ do
                renderUserStateBubble usrUserState
                H.br
                renderKYCtateBubble usrAptoKYCStatus
              H.td ! A.class_ tableCellClasses $ do
                case usrBankVerified of
                  Just True  -> H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ "Verified"
                  Just False -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Unverified"
                  Nothing    -> mempty
                H.span " "
                case usrBankVerifedAmounts of
                  Nothing           -> mempty
                  Just [0.31, 0.72] -> H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Plaid"
                  Just [_   , _   ] -> H.span ! A.class_ (badgeClasses <> " bg-blue-100 text-blue-800") $ "Plaid"
                  Just amounts      -> mapM_
                    (\a -> do
                      H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ toHtml a
                      H.span " "
                    )
                    amounts
              H.td ! A.class_ tableCellClasses $ do
                let fullName = fromMaybe "" usrFirstName <> " " <> fromMaybe "" usrLastName
                toHtml $ if T.length fullName > 50 then T.take 47 fullName <> "..." else fullName
              H.td ! A.class_ tableCellClasses $ do
                case usrBankRouting of
                  Nothing               -> mempty
                  Just (RedactedText t) -> H.div $ do
                    H.span "Routing# "
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ toHtml t

                case usrBankAcount of
                  Nothing               -> mempty
                  Just (RedactedText t) -> H.div $ do
                    H.span "Account# "
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ toHtml t
              H.td ! A.class_ tableCellClasses $ do
                case usrBankName of
                  Nothing -> mempty
                  Just t  -> H.div $ do
                    H.span "Bank "
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ toHtml t

                case usrBankAccountName of
                  Nothing -> mempty
                  Just t  -> H.div $ do
                    H.span "Account "
                    H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ toHtml t
              H.td ! A.class_ tableCellClasses $ do
                case usrBankType of
                  Nothing -> mempty
                  Just (Depository Checking) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Checking"
                  Just (Depository Savings) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Savings"
                  Just (Depository Prepaid) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Prepaid"
                  Just (Depository DepositoryTypeUnknown) -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Unknown"
                  Just _  -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Other"
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ case usrPhone of
                  Just (PhoneNumber p) -> p
                  Nothing              -> ""
              H.td ! A.class_ tableCellClasses $ do
                foldMap showDate usrDOB
              H.td ! A.class_ tableCellClasses $ do
                case usrSSN of
                  Nothing -> mempty
                  Just _  -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Redacted"
              H.td ! A.class_ tableCellClasses $ do
                foldMap showDateTime usrDislcosureOk
              H.td ! A.class_ tableCellClasses $ do
                foldMap showDateTime usrConstentOk
              H.td ! A.class_ tableCellClasses $ do
                H.div $ do
                  toHtml $ fromMaybe "" usrAddressStreet
                when
                  (isJust usrAddressStreet2)
                  (do
                    H.div $ do
                      toHtml $ fromMaybe "" usrAddressStreet2
                  )
                H.div $ do
                  H.span $ toHtml $ fromMaybe "" usrAddressCity
                  H.span ", "
                  H.span $ toHtml $ fromMaybe "" usrAddressState
                  H.span " "
                  H.span $ toHtml $ fromMaybe "" usrAddressZip
              H.td ! A.class_ tableCellClasses $ do
                case usrDwollaId of
                  Nothing -> mempty
                  Just t  -> H.div . toHtml $ T.takeEnd 20 t

                case usrDwollaFundingId of
                  Nothing -> mempty
                  Just t  -> H.div . toHtml $ T.takeEnd 20 t
              H.td ! A.class_ tableCellClasses $ do
                renderEmail usrEmail
              H.td ! A.class_ tableCellClasses $ do
                renderMessageLink usrMsgSource

      mapM_ createRow revs

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderBalances :: [(PlaidBalanceRow, UTCTime)] -> Html
renderBalances bals = do
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Date"
        H.th ! A.class_ tableHeaderCellClasses $ "Access Time"
        H.th ! A.class_ tableHeaderCellClasses $ "Current Balance"
        H.th ! A.class_ tableHeaderCellClasses $ "Available Balance"
        H.th ! A.class_ tableHeaderCellClasses $ "Account Type"
        H.th ! A.class_ tableHeaderCellClasses $ "Account Limit"
        H.th ! A.class_ tableHeaderCellClasses $ "Account Name"
        H.th ! A.class_ tableHeaderCellClasses $ "Account Official Name"
    H.tbody $ do
      let createRow (PlaidBalanceRow { pbAccount = Account {..}, ..}, time) = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                showDateTime time
              H.td ! A.class_ tableCellClasses $ do
                toHtml pbAccessTime
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ showCurr accountCurrentBalance
              H.td ! A.class_ tableCellClasses $ do
                case accountAvailableBalance of
                  Nothing -> mempty
                  Just b  -> toHtml $ showCurr b
              H.td ! A.class_ tableCellClasses $ do
                case accountType of
                  Depository Checking -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Checking"
                  Depository Savings -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Savings"
                  Depository Prepaid -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Prepaid"
                  Depository DepositoryTypeUnknown -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "DepositoryTypeUnknown"
                  _ -> H.span ! A.class_ (badgeClasses <> " bg-red-100 text-red-800") $ "Other"
              H.td ! A.class_ tableCellClasses $ do
                case accountLimit of
                  Nothing -> mempty
                  Just b  -> toHtml $ showCurr b
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ fromMaybe "" accountName
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ fromMaybe "" accountOfficialName

      mapM_ createRow bals

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTokens :: UTCTime -> UserID -> [UserToken] -> Html
renderTokens now (UserID u) tokens = do
  let uid = toValue $ toText u

  H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
    H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
      H.form ! A.class_ "inline" ! A.method "post" ! A.action ("/user/" <> uid <> "/token/create/email") $ do
        H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
          H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
            "Send Email Token"
      H.form ! A.class_ "inline" ! A.method "post" ! A.action ("/user/" <> uid <> "/token/create/phone") $ do
        H.span ! A.class_ "inline-flex rounded-md shadow-sm ml-1" $ do
          H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
            "Send Phone Token"
      H.form ! A.class_ "inline" ! A.method "post" ! A.action ("/user/" <> uid <> "/token/create/push") $ do
        H.span ! A.class_ "inline-flex rounded-md shadow-sm ml-1" $ do
          H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
            "Send Push Token"

    H.form ! A.class_ "block" ! A.method "post" ! A.action ("/user/" <> uid <> "/token/verify/phone") $ do
      H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
        H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
          H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
            H.input ! A.name "token" ! A.id "tokenInput" ! A.placeholder "123456" ! A.type_ "decimal" ! A.autocomplete "off" ! A.class_ "form-input sm:text-sm sm:leading-5"
            H.span ! A.class_ "inline-flex rounded-md shadow-sm ml-1" $ do
              H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                "Verify Token"

  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Created On"
        H.th ! A.class_ tableHeaderCellClasses $ "Medium"
        H.th ! A.class_ tableHeaderCellClasses $ "Token"
        H.th ! A.class_ tableHeaderCellClasses $ "Address"
        H.th ! A.class_ tableHeaderCellClasses $ "Expires"
        H.th ! A.class_ tableHeaderCellClasses $ "Expired"
    H.tbody $ do
      let createRow UserToken {..} = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                showDateTime tokCreatedOn
              H.td ! A.class_ tableCellClasses $ do
                case tokToken of
                  EmailToken _ _ -> "Email"
                  PhoneToken _ _ -> "Phone"
                  PushToken _    -> "Push"
              H.td ! A.class_ tableCellClasses $ do
                case tokToken of
                  EmailToken _ t -> toHtml t
                  PhoneToken _ t -> toHtml t
                  PushToken t    -> toHtml t
              H.td ! A.class_ tableCellClasses $ do
                case tokToken of
                  EmailToken (EmailAddress t) _ -> toHtml t
                  PhoneToken (PhoneNumber  t) _ -> "+1" <> toHtml t
                  PushToken _                   -> "Push Notification"
              H.td ! A.class_ tableCellClasses $ do
                maybe "No expiration" showDateTime tokExpiresAt
              H.td ! A.class_ tableCellClasses $ do
                case tokExpiresAt of
                  Nothing     -> "No"
                  Just expire -> if expire < now then "Yes" else "No"

      mapM_ createRow tokens

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderAssessments :: UserID -> [KYCAssesment] -> Html
renderAssessments (UserID uid) asses = do
  let createRow KYCAssesment {..} = do
        H.tr ! A.class_ "bg-white" $ do
          H.td ! A.class_ tableFirstCellClasses $ do
            showDateTime createdAt
          H.td ! A.class_ tableCellClasses $ do
            if kycPassed then "Passed" else "Failed"
          H.td ! A.class_ tableCellClasses $ do
            mapM_
              (\case
                PhoneScoreLow          -> H.div "Phone Score Low"
                AddressScoreLow        -> H.div "Address Score Low"
                SSNScoreLow            -> H.div "SSN Score Low"
                NameScoreLow           -> H.div "Name Score Low"
                DOBScoreLow            -> H.div "DOB Score Low"
                IdentityTheftRisk      -> H.div "Identity Theft Risk"
                WatchlistNeedsReview   -> H.div "OFAC Needs review"
                WatchlistRejected      -> H.div "OFAC Rejected"
                DocumentationIncorrect -> H.div "Bad docs provided"
              )
              failureReasons
          H.td ! A.class_ tableCellClasses $ do
            toHtml scoreOverall
          H.td ! A.class_ tableCellClasses $ do
            maybe "None" toHtml scorePhone
          H.td ! A.class_ tableCellClasses $ do
            maybe "None" toHtml scoreAddress
          H.td ! A.class_ tableCellClasses $ do
            maybe "None" toHtml scoreSSN
          H.td ! A.class_ tableCellClasses $ do
            maybe "None" toHtml scoreName
          H.td ! A.class_ tableCellClasses $ do
            toHtml countPhone
          H.td ! A.class_ tableCellClasses $ do
            toHtml countAddresss
          H.td ! A.class_ tableCellClasses $ do
            toHtml countSSN
          H.td ! A.class_ tableCellClasses $ do
            toHtml countName

  H.div ! A.class_ "bg-white shadow overflow-hidden sm:rounded-md mb-4" $ do
    H.ul $ do
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "KYC Actions "
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> toValue (toText uid) <> "/kyc/passed"
              H.form ! A.action url ! A.method "post" $ do
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Approve KYC"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> toValue (toText uid) <> "/kyc/rejected"
              H.form ! A.action url ! A.method "post" $ do
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> redColorButton) $ do
                  "Reject KYC"
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              let url = "/user/" <> toValue (toText uid) <> "/kyc/autoverifyfailed"
              H.form ! A.action url ! A.method "post" $ do
                H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
                  "Move to Verify Failed KYC"
      H.li $ do
        H.div ! A.class_ "px-4 py-4 sm:px-6" $ do
          H.div ! A.class_ "flex items-center justify-between" $ do
            H.div ! A.class_ "ml-1 font-normal text-gray-500" $ do
              "KYC Actions "
            H.div ! A.class_ "ml-2 flex-shrink-0 flex" $ do
              H.a ! A.href ("/user/" <> toValue (toText uid) <> "/kyc/check") ! A.class_ (mediumButton <> indigoColorButton <> " mt-1") $ do
                "Run new kyc"

  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "Run At"
        H.th ! A.class_ tableHeaderCellClasses $ "P/F"
        H.th ! A.class_ tableHeaderCellClasses $ "Failure Reason"
        H.th ! A.class_ tableHeaderCellClasses $ "Overall Score"
        H.th ! A.class_ tableHeaderCellClasses $ "Phone Score"
        H.th ! A.class_ tableHeaderCellClasses $ "Address Score"
        H.th ! A.class_ tableHeaderCellClasses $ "SSN Score"
        H.th ! A.class_ tableHeaderCellClasses $ "Name Score"
        H.th ! A.class_ tableHeaderCellClasses $ "Phone Count"
        H.th ! A.class_ tableHeaderCellClasses $ "Address Count"
        H.th ! A.class_ tableHeaderCellClasses $ "SSN Count"
        H.th ! A.class_ tableHeaderCellClasses $ "Name Count"
    H.tbody $ do
      mapM_ createRow asses

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderNotes :: UserID -> Text -> Html
renderNotes (UserID userId) txt = do
  let uid = toValue (toText userId)
  let url = "/user/" <> uid <> "/notes"

  H.form ! A.action url ! A.method "post" $ do
    H.div $ do
      H.textarea ! A.name "note" ! A.placeholder "No notes yet" ! A.spellcheck "true" ! A.wrap "soft" ! A.rows "5" ! A.class_ "w-full resize-y border rounded-lg px-1.5 py-0.5" $ do
        toHtml txt
    H.div $ do
      H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton <> " mt-2") $ do
        "Update notes"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderJournalActions :: UserID -> Html
renderJournalActions (UserID uid) = do
  H.div $ H.form ! A.method "post" ! A.action (toValue $ "/ledger/journals/user/" <> toText uid) $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        -- First line
        H.label ! A.for "name" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "Journal Type"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.select ! A.name "jtype" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5" $ do
            H.option ! A.value "PayTgthr" $ "Pay Tgthr"
            H.option ! A.value "StashTgthr" $ "Stash Tgthr"
            H.option ! A.value "SaveTgthr" $ "Save Tgthr"
            H.option ! A.value "FundingSource" $ "Funding Source"
        H.label ! A.for "dwolla" ! A.class_ "block text-sm font-medium leading-5 text-gray-700" $ do
          "Dwolla ID"
        H.div ! A.class_ "mt-1 relative rounded-md shadow-sm" $ do
          H.input ! A.name "dwolla" ! A.id "dwolla" ! A.placeholder "https://..." ! A.type_ "text" ! A.autocomplete "off" ! A.class_ "form-input block w-full sm:text-sm sm:leading-5"

        --Submit
        H.div ! A.class_ "mt-2" $ do
          H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
            H.button ! A.type_ "submit" ! A.class_ (mediumButton <> indigoColorButton) $ do
              "New journal"
