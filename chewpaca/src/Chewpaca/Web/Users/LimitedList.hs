{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.Users.LimitedList where

import           Chewpaca.Tailwind.Classes      ( badgeClasses
                                                , dataTable
                                                , tableCellClasses
                                                , tableFirstCellClasses
                                                , tableHeaderCellClasses
                                                , tableLinkClasses
                                                )
import           Chewpaca.Tailwind.Frame        ( renderMessageLink
                                                , showDate
                                                , showDateTime
                                                )
import           Chewpaca.Web.Users             ( generateLinkToUserPage
                                                , renderEmail
                                                , renderEmailValue
                                                , renderKYCtateBubble
                                                , renderUserID
                                                , renderUserStateBubble
                                                )
import           Control.Monad                  ( when )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.User             ( PhoneNumber(..)
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
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A



-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderListOfUsers :: [(UserModel, UTCTime)] -> Html
renderListOfUsers users = do

  H.div $ do
    dataTable $ do
      H.thead $ do
        H.tr $ do
          H.th ! A.class_ tableHeaderCellClasses $ "Id"
          H.th ! A.class_ tableHeaderCellClasses $ "Name"
          H.th ! A.class_ tableHeaderCellClasses $ "Email"
          H.th ! A.class_ tableHeaderCellClasses $ "Rev"
          H.th ! A.class_ tableHeaderCellClasses $ "Updated on"
          H.th ! A.class_ tableHeaderCellClasses $ "Account Status"
          H.th ! A.class_ tableHeaderCellClasses $ "Bank Verified"
          H.th ! A.class_ tableHeaderCellClasses $ "Bank #s"
          H.th ! A.class_ tableHeaderCellClasses $ "Bank Names"
          H.th ! A.class_ tableHeaderCellClasses $ "Bank Type"
          H.th ! A.class_ tableHeaderCellClasses $ "Phone"
          H.th ! A.class_ tableHeaderCellClasses $ "DOB"
          H.th ! A.class_ tableHeaderCellClasses $ "SSN"
          H.th ! A.class_ tableHeaderCellClasses $ "Created"
          H.th ! A.class_ tableHeaderCellClasses $ "First Signin"
          H.th ! A.class_ tableHeaderCellClasses $ "Disclosure"
          H.th ! A.class_ tableHeaderCellClasses $ "Consent"
          H.th ! A.class_ tableHeaderCellClasses $ "Activated"
          H.th ! A.class_ tableHeaderCellClasses $ "Card Created"
          H.th ! A.class_ tableHeaderCellClasses $ "Card Activated"
          H.th ! A.class_ tableHeaderCellClasses $ "Address"
          H.th ! A.class_ tableHeaderCellClasses $ "Dwolla Links"
          H.th ! A.class_ tableHeaderCellClasses $ "Message"
      H.tbody $ do
        let createRow (UserModel {..}, time) = do
              H.tr ! A.class_ "bg-white" $ do
                H.td ! A.class_ tableFirstCellClasses $ do
                  H.a ! A.href (generateLinkToUserPage usrUserID) ! A.class_ tableLinkClasses $ do
                    renderUserID usrUserID
                H.td ! A.class_ tableCellClasses $ do
                  let fullName = fromMaybe "" usrFirstName <> " " <> fromMaybe "" usrLastName
                  toHtml $ if T.length fullName > 50 then T.take 47 fullName <> "..." else fullName
                H.td ! A.class_ tableCellClasses $ do
                  H.span ! A.class_ "mr-1" $ renderEmail usrEmail
                  H.span ! A.class_ "inline-flex rounded-md shadow-sm" $ do
                    let jscript = "navigator.clipboard.writeText('" <> renderEmailValue usrEmail <> "')"
                    H.button ! A.onclick jscript ! A.type_ "button" ! A.class_ "inline-flex items-center px-2.5 py-1.5 border border-gray-300 text-xs leading-4 font-medium rounded text-gray-700 bg-white hover:text-gray-500 focus:outline-none focus:border-blue-300 focus:shadow-outline-blue active:text-gray-800 active:bg-gray-50 transition ease-in-out duration-150" $ do
                      "Copy"
                H.td ! A.class_ tableCellClasses $ do
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
                    Nothing      -> mempty
                    Just amounts -> mapM_
                      (\a -> do
                        H.span ! A.class_ (badgeClasses <> " bg-green-100 text-green-800") $ toHtml a
                        H.span " "
                      )
                      amounts
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

                  case usrBankName of
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
                  case usrDOB of
                    Nothing  -> mempty
                    Just dob -> showDate dob
                H.td ! A.class_ tableCellClasses $ do
                  case usrSSN of
                    Nothing -> mempty
                    Just _  -> H.span ! A.class_ (badgeClasses <> " bg-gray-100 text-gray-800") $ "Redacted"
                H.td ! A.class_ tableCellClasses $ do
                  showDateTime usrCreatedOn
                H.td ! A.class_ tableCellClasses $ do
                  case usrFirstSignIn of
                    Nothing    -> mempty
                    Just aTime -> showDateTime aTime
                H.td ! A.class_ tableCellClasses $ do
                  case usrDislcosureOk of
                    Nothing    -> mempty
                    Just aTime -> showDateTime aTime
                H.td ! A.class_ tableCellClasses $ do
                  case usrConstentOk of
                    Nothing    -> mempty
                    Just aTime -> showDateTime aTime
                H.td ! A.class_ tableCellClasses $ do
                  case usrBecameActiveOn of
                    Nothing    -> mempty
                    Just aTime -> showDateTime aTime
                H.td ! A.class_ tableCellClasses $ do
                  case usrCardCreatedOn of
                    Nothing    -> mempty
                    Just aTime -> showDateTime aTime
                H.td ! A.class_ tableCellClasses $ do
                  case usrCardActivatedOn of
                    Nothing    -> mempty
                    Just aTime -> showDateTime aTime
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
                  renderMessageLink usrMsgSource

        mapM_ createRow users
