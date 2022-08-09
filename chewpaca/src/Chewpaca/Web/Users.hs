{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards #-}

module Chewpaca.Web.Users where

import           Chewpaca.Tailwind.Frame        ( showDateTime )
import           Data.Aeson                     ( ToJSON(toJSON)
                                                , Value(..)
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( toText )
import           Shared.Models.Card             ( CardStatus(..) )
import           Shared.Models.KYC
import           Shared.Models.User             ( EmailAddress(..)
                                                , PhoneNumber(..)
                                                , UserID(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , AttributeValue
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderKYCtateBubble :: Maybe KycStatus -> Html
renderKYCtateBubble Nothing          = mempty
renderKYCtateBubble (Just kycStatus) = do
  case kycStatus of
    Passed             -> mempty
    AutoVerifyFailed _ -> H.span ! A.class_ "px-2 my-1 inline-flex text-xs leading-5 font-semibold rounded-full bg-red-100 text-red-800" $ "Auto KYC Failed"
    Rejected         _ -> H.span ! A.class_ "px-2 my-1 inline-flex text-xs leading-5 font-semibold rounded-full bg-red-100 text-red-800" $ "Rejected"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderUserStateBubble :: UserState -> Html
renderUserStateBubble userState = case userState of
  UserCreated         -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-gray-100 text-gray-800" $ "User Created"
  UserWaitingOnPII    -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-blue-100 text-blue-800" $ "PII Entry"
  UserWaitingOnKYC    -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-red-100 text-red-800" $ "Waiting on KYC Check"
  UserKYCDelay        -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-yellow-100 text-yellow-800" $ "Auto KYC Failed"
  UserActive          -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-green-100 text-green-800" $ "User Active"
  UserUpdated         -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-red-100 text-red-800" $ "PII Updated"
  UserUpdatedKYCDelay -> H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-yellow-100 text-yellow-800" $ "Auto KYC Failed"
  UserClosed reason   -> do
    H.span ! A.class_ "px-2 inline-flex text-xs leading-5 font-semibold rounded-full bg-purple-100 text-purple-800" $ do
      "Closed"
    H.br
    H.span ! A.class_ "px-2 my-1 inline-flex text-xs leading-5 font-semibold rounded-full bg-purple-100 text-purple-800" $ do
      toHtml (show reason)

renderEmail :: EmailAddress -> Html
renderEmail (EmailAddress t) = toHtml t

renderEmailValue :: EmailAddress -> AttributeValue
renderEmailValue (EmailAddress t) = toValue t

renderUserID :: UserID -> Html
renderUserID (UserID u) = case T.splitOn "-" $ toText u of
  []             -> "Error: Can't split uid"
  (firstSec : _) -> toHtml (firstSec <> "...")

extUserID :: UserID -> Html
extUserID (UserID u) = toHtml $ toText u

valueUserID :: UserID -> AttributeValue
valueUserID (UserID u) = toValue $ toText u

renderTime :: UTCTime -> Html
renderTime time = case toJSON time of
  String t -> toHtml t
  _        -> mempty

generateLinkToUserPage :: UserID -> AttributeValue
generateLinkToUserPage (UserID u) = toValue $ "/user/" <> toText u

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderUsers :: [UserModel] -> Html
renderUsers users =
  let firstCellClasses = "px-6 py-4 whitespace-no-wrap text-sm leading-5 font-medium text-gray-900"
      cellClasses      = "px-6 py-4 whitespace-no-wrap text-sm leading-5 text-gray-500"
      linkClasses      = "text-indigo-600 hover:text-indigo-900"
      isActiveAccount UserModel {..} = usrUserState == UserActive
      waitingOnDocs UserModel {..} = usrUserState == UserKYCDelay || usrUserState == UserUpdatedKYCDelay
      isOnlyCreated UserModel {..} = usrUserState == UserCreated
      waitingToCompletePII UserModel {..} = usrUserState == UserWaitingOnPII
      hasCardIssued UserModel {..} = isJust usrAptoCardStatus
      hasActiveCard UserModel {..} = usrAptoCardStatus == Just CardActive || usrAptoCardStatus == Just CardClosed
      bankVerified UserModel {..} = usrBankVerified == Just True && usrUserState == UserActive
      wasSentToApto UserModel {..} = isJust usrAptoCardholderID
  in  do
        H.div $ do
          let totalUsers      = length users
          let activeUsers     = length $ filter isActiveAccount users
          let docWaitingUsers = length $ filter waitingOnDocs users
          let sentToAptoUsers = length $ filter wasSentToApto users

          H.h3 ! A.class_ "text-lg leading-6 font-medium text-gray-900" $ do
            "Quick Stats"
          H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-5" $ do
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Total Accounts"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ toHtml totalUsers
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Active Accounts"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let activePercent :: Int = case totalUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral activeUsers / fromIntegral totalUsers * (100.0 :: Double)
                    H.span $ toHtml activeUsers
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml activePercent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Waiting on manual KYC"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let docWaitingPerent :: Int = case totalUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral docWaitingUsers / fromIntegral sentToAptoUsers * (100.0 :: Double)
                    H.span $ toHtml docWaitingUsers
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml docWaitingPerent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Still completing forms"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let stillComletingForms = length $ filter waitingToCompletePII users
                    let stillComletingFormsPercent :: Int = case totalUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral stillComletingForms / fromIntegral totalUsers * (100.0 :: Double)
                    H.span $ toHtml stillComletingForms
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml stillComletingFormsPercent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Was only invited"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let wasCreatedOnly = length $ filter isOnlyCreated users
                    let wasCreatedOnlyPercent :: Int = case totalUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral wasCreatedOnly / fromIntegral totalUsers * (100.0 :: Double)
                    H.span $ toHtml wasCreatedOnly
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml wasCreatedOnlyPercent
                      "%"
          -- row 2
          H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-4" $ do
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Has card issued"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let isHoldingCard = length $ filter hasCardIssued users
                    let isHoldingCardPercent :: Int = case totalUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral isHoldingCard / fromIntegral totalUsers * (100.0 :: Double)
                    H.span $ toHtml isHoldingCard
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml isHoldingCardPercent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Has activated card"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let isHoldingActiveCard = length $ filter hasActiveCard users
                    let isHoldingActiveCardPercent :: Int = case totalUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral isHoldingActiveCard / fromIntegral totalUsers * (100.0 :: Double)
                    H.span $ toHtml isHoldingActiveCard
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml isHoldingActiveCardPercent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Active user w/ verified bank"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let hasBankVerified = length $ filter bankVerified users
                    let hasBankVerifiedPercent :: Int = case activeUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral hasBankVerified / fromIntegral activeUsers * (100.0 :: Double)
                    H.span $ toHtml hasBankVerified
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml hasBankVerifiedPercent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Active user w/o verified bank"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let bankIsntVerified UserModel {..} = usrBankVerified /= Just True && usrUserState == UserActive
                    let notBankVerified = length $ filter bankIsntVerified users
                    let notBankVerifiedPercent :: Int = case activeUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral notBankVerified / fromIntegral activeUsers * (100.0 :: Double)
                    H.span $ toHtml notBankVerified
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml notBankVerifiedPercent
                      "%"
          -- row 3
          H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-4" $ do
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Under Review"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let filteringFn UserModel {..} = case usrAptoKYCStatus of
                          Just (AutoVerifyFailed _) -> True
                          _                         -> False
                    let count = length $ filter filteringFn users
                    let percent :: Int = case docWaitingUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral count / fromIntegral docWaitingUsers * (100.0 :: Double)
                    H.span $ toHtml count
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml percent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "Error Running"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let filteringFn UserModel {..} = case usrAptoKYCStatus of
                          Just (AutoVerifyFailed _) -> True
                          _                         -> False
                    let count = length $ filter filteringFn users
                    let percent :: Int = case docWaitingUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral count / fromIntegral docWaitingUsers * (100.0 :: Double)
                    H.span $ toHtml count
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml percent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "SSN Mismatch"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let filteringFn UserModel {..} = case usrAptoKYCStatus of
                          Just (AutoVerifyFailed []) -> True
                          _                          -> False
                    let count = length $ filter filteringFn users
                    let percent :: Int = case docWaitingUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral count / fromIntegral docWaitingUsers * (100.0 :: Double)
                    H.span $ toHtml count
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml percent
                      "%"
            H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
              H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
                H.dl $ do
                  H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ "SSN Invalid"
                  H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
                    let filteringFn UserModel {..} = case usrAptoKYCStatus of
                          Just (AutoVerifyFailed []) -> True
                          _                          -> False
                    let count = length $ filter filteringFn users
                    let percent :: Int = case docWaitingUsers of
                          0 -> 0
                          _ -> floor $ fromIntegral count / fromIntegral docWaitingUsers * (100.0 :: Double)
                    H.span $ toHtml count
                    H.span " "
                    H.span ! A.class_ "inline-flex items-baseline px-2.5 py-0.5 rounded-full text-sm font-medium leading-5 bg-blue-100 text-blue-800 md:mt-2 lg:mt-0" $ do
                      toHtml percent
                      "%"
        -- -- --
        H.h3 ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
          "User List"
        H.div ! A.class_ "flex flex-col" $ do
          H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
            H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
              H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
                H.table ! A.class_ "min-w-full divide-y divide-gray-200" $ do
                  H.thead $ do
                    H.tr $ do
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "ID"
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "Rev"
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "Email"
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "Phone"
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "Account Status"
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "Name"
                      H.th ! A.class_ "px-6 py-3 bg-gray-50 text-left text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                        "Created on"
                  H.tbody $ do
                    let createRow UserModel {..} = do
                          H.tr ! A.class_ "bg-white" $ do
                            H.td ! A.class_ firstCellClasses $ do
                              H.a ! A.href (generateLinkToUserPage usrUserID) ! A.class_ linkClasses $ do
                                renderUserID usrUserID
                            H.td ! A.class_ cellClasses $ do
                              toHtml usrRevision
                            H.td ! A.class_ cellClasses $ do
                              renderEmail usrEmail
                            H.td ! A.class_ cellClasses $ do
                              case usrPhone of
                                Nothing              -> mempty
                                Just (PhoneNumber p) -> toHtml p
                            H.td ! A.class_ cellClasses $ do
                              renderUserStateBubble usrUserState
                              H.br
                              renderKYCtateBubble usrAptoKYCStatus
                            H.td ! A.class_ cellClasses $ do
                              let fullName = fromMaybe "" usrFirstName <> " " <> fromMaybe "" usrLastName
                              toHtml $ if T.length fullName > 50 then T.take 47 fullName <> "..." else fullName
                            H.td ! A.class_ cellClasses $ do
                              showDateTime usrCreatedOn
                    mapM_ createRow users
