{-# LANGUAGE RecordWildCards #-}
{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module Chewpaca.Tailwind.Frame where

import           Chewpaca.Tailwind.Classes      ( tableLinkClasses )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.LocalTime            ( utcToZonedTime )
import           Data.UUID                      ( toText )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids
import           Shared.Models.Rewards.Boost
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Blaze.Internal            ( Attribute
                                                , AttributeValue
                                                , attribute
                                                )
import qualified Text.Blaze.Svg11              as S
import qualified Text.Blaze.Svg11.Attributes   as SA
import           Text.Printf                    ( printf )

data UserPageTypes
  = MainList
  | SearchBar
  | SearchResults Text
  | GeoStats
  | CardNotActive
  | UnderReviewList
  | SpecificUser (Maybe Text, UserID)
  deriving (Eq, Show)

data TransactionPageTypes
  = MainTrxList
  | TransactionStats
  | SpecificTransaction (TransactionId, Maybe Text)
  deriving (Eq, Show)

data CurrentSection
  = Homepage
  | Users UserPageTypes
  | Groups (Maybe GroupId)
  | Transactions TransactionPageTypes
  | Payments (Maybe PaymentId)
  | Dashboard
  | LedgerJournal
  | Rewards
  | Reward RewardBoost
  deriving (Eq, Show)

showCurr :: Currency -> String
showCurr (Currency "USD" val) =
  let f :: Double = fromRational val
  in  if fromIntegral (floor f :: Int) == f
        then printf "$%.0f" f
        else printf "$%.2f" f
showCurr (Currency iso val) =
  let f :: Double = fromRational val
  in  if fromIntegral (floor f :: Int) == f
        then printf "%s %.0f" iso f
        else printf "%s %.2f" iso f

showDateTime :: UTCTime -> Html
showDateTime time =
  toHtml $ formatTime defaultTimeLocale "%m-%d-%Y %R" $ utcToZonedTime
    timeZone
    time
  where timeZone = read "PDT"

showDateWithoutTZ :: UTCTime -> Html
showDateWithoutTZ time = toHtml $ formatTime defaultTimeLocale "%m-%d-%Y" time

showDate :: UTCTime -> Html
showDate time =
  toHtml $ formatTime defaultTimeLocale "%m-%d-%Y" $ utcToZonedTime timeZone
                                                                    time
  where timeZone = read "PDT"

ariaLabel :: AttributeValue -> Attribute
ariaLabel = attribute "aria-label" " aria-label=\""

ariaHasPopup :: AttributeValue -> Attribute
ariaHasPopup = attribute "aria-haspopup" " aria-haspopup=\""

-- inline brittany config for width
-- brittany-next-binding --columns 400
renderMessageLink :: MessageID -> Html
renderMessageLink (MessageID uuid) = do
  case T.splitOn "-" $ toText uuid of
    []          -> "Error: Can't split uuid"
    (first : _) -> H.a ! A.href (toValue $ "/message?id=" <> toText uuid) ! A.class_ tableLinkClasses $ do
      toHtml $ first <> "..."

-- inline brittany config for width
-- brittany-next-binding --columns 400
emptyShell :: Html
emptyShell = do
  H.div ! A.class_ "max-w-7xl mx-auto px-4 sm:px-6 md:px-8" $ do
    H.h1 ! A.class_ "text-2xl font-semibold text-gray-900" $ "Stuff goes here"
  H.div ! A.class_ "max-w-7xl mx-auto px-4 sm:px-6 md:px-8" $ do
    H.div ! A.class_ "py-4" $ do
      H.div ! A.class_ "border-4 border-dashed border-gray-200 rounded-lg h-96" $ do
        mempty

-- inline brittany config for width
-- brittany-next-binding --columns 500
breadcrumbHtml :: CurrentSection -> Html
breadcrumbHtml currPage =
  let (pagetitle, crumbs) = case currPage of
        Homepage      -> ("Chewpaca", [])
        Users MainList -> ("Users", [("/users", "Users")])
        Users SearchBar -> ("User Search", [("/users/search", "User Search")])
        Users (SearchResults t) -> ("Search results for " <> toHtml t, [("/users/search", "User Search"), ("/users/search/text?q=" <> toValue t, toHtml t)])
        Users GeoStats -> ("Geogrpahy report", [("/users", "Users"), ("/users/geo", "Geo Report")])
        Users CardNotActive -> ("Card not activated user list", [("/users", "Users"), ("/users/cardcreated", "Card not active")])
        Users UnderReviewList -> ("Under Review list", [("/users", "Users"), ("/users/underreview", "Under Review")])
        Users (SpecificUser (name, UserID u)) -> (toHtml $ fromMaybe (toText u) name, [("/users", "Users"), (toValue $ "/user/" <> toText u, toHtml $ fromMaybe (toText u) name)])
        Transactions MainTrxList -> ("Transactions", [("/transactions", "Trxs")])
        Transactions TransactionStats -> ("Transaction Stats", [("/transactions", "Transactions"), ("/transactions/stats", "Stats")])
        Transactions (SpecificTransaction (TransactionId t, name)) -> (toHtml $ "Transaction " <> T.take 20 (fromMaybe (toText t) name), [("/transactions", "Trxs"), (toValue $ "/transaction/" <> toText t, toHtml $ toText t)])
        Payments Nothing -> ("Payments", [("/payments", "Payments")])
        Payments (Just (PaymentId p)) -> (toHtml $ "Payment " <> toText p, [("/payments", "Payments"), ("/payment/" <> toValue (toText p), toHtml (toText p))])
        Groups Nothing -> ("Groups", [("/groups", "Groups")])
        Groups (Just (GroupId g)) -> (toHtml $ "Group " <> toText g, [("/groups", "Groups"), ("/group/" <> toValue (toText g), toHtml (toText g))])
        Dashboard     -> ("Dashboard", [("/dashboard", "Dashboard")])
        LedgerJournal -> ("Ledger Journals", [("/ledger/journals", "Ledger Journals")])
        Rewards       -> ("Rewards", [("/rewards/list", "Rewards")])
        Reward RewardBoost { boostId = RewardId i, ..} -> (toHtml boostName, [(toValue $ "/rewards/reward/" <> toText i, toHtml boostName)])
      generateCrumb (url, title) = do
        S.svg ! SA.class_ "flex-shrink-0 mx-2 h-5 w-5 text-gray-400" ! SA.viewbox "0 0 20 20" ! SA.fill "currentColor" $ do
          S.path ! SA.fillRule "evenodd" ! SA.clipRule "evenodd" ! SA.d "M7.293 14.707a1 1 0 010-1.414L10.586 10 7.293 6.707a1 1 0 011.414-1.414l4 4a1 1 0 010 1.414l-4 4a1 1 0 01-1.414 0z"
        H.a ! A.href url ! A.class_ "text-gray-500 hover:text-gray-700 transition duration-150 ease-in-out" $ title
  in  do
        H.div ! A.class_ "self-center" $ do
          H.div $ do
            H.nav ! A.class_ "hidden" $ do
              H.a ! A.href "#" ! A.class_ "flex items-center text-sm leading-5 font-medium text-gray-500 hover:text-gray-700 transition duration-150 ease-in-out" $ do
                "Back"
            H.nav ! A.class_ "hidden sm:flex items-center text-sm leading-5 font-medium" $ do
              H.a ! A.href "/" ! A.class_ "text-gray-500 hover:text-gray-700 transition duration-150 ease-in-out" $ "Chewy"
              mapM_ generateCrumb crumbs
          H.div ! A.class_ "mt-2 md:flex md:items-center md:justify-between" $ do
            H.div ! A.class_ "flex-1 min-w-0" $ do
              H.h2 ! A.class_ "text-2xl font-bold leading-7 text-gray-900 sm:text-3xl sm:leading-9 sm:truncate" $ do
                pagetitle

data SidebarStyle
  = MobileSidebar
  | DesktopSidebar
  deriving (Eq, Show)

-- inline brittany config for width
-- brittany-next-binding --columns 400
sidebarHtml :: SidebarStyle -> CurrentSection -> Html
sidebarHtml sidebarStyle curr =
  let regularClasses = if sidebarStyle == MobileSidebar
        then "group flex items-center px-2 py-2 text-base leading-6 font-medium rounded-md text-indigo-300 hover:text-white hover:bg-indigo-700 focus:outline-none focus:text-white focus:bg-indigo-700 transition ease-in-out duration-150"
        else "group flex items-center px-2 py-2 text-sm leading-5 font-medium text-indigo-300 rounded-md hover:text-white hover:bg-indigo-700 focus:outline-none focus:text-white focus:bg-indigo-700 transition ease-in-out duration-150"
      selectedClasses = if sidebarStyle == MobileSidebar
        then "group flex items-center px-2 py-2 text-base leading-6 font-medium rounded-md text-white bg-indigo-900 focus:outline-none focus:bg-indigo-700 transition ease-in-out duration-150"
        else "group flex items-center px-2 py-2 text-sm leading-5 font-medium text-white rounded-md bg-indigo-900 focus:outline-none focus:bg-indigo-700 transition ease-in-out duration-150"
      currPageIs c = if curr == c then selectedClasses else regularClasses
      userSectionClasses = case curr of
        Users MainList -> selectedClasses
        _              -> regularClasses
      groupSectionClasses = case curr of
        Groups _ -> selectedClasses
        _        -> regularClasses
      paymentSectionClasses = case curr of
        Payments _ -> selectedClasses
        _          -> regularClasses
      transactionSectionClasses = case curr of
        Transactions MainTrxList -> selectedClasses
        _                        -> regularClasses
      geoReportClasses = case curr of
        Users GeoStats -> selectedClasses
        _              -> regularClasses
      cardNotActiveClasses = case curr of
        Users CardNotActive -> selectedClasses
        _                   -> regularClasses
      cardUnderReviewClasses = case curr of
        Users UnderReviewList -> selectedClasses
        _                     -> regularClasses
      sepcificUserClasses = case curr of
        Users (SpecificUser _) -> selectedClasses
        _                      -> regularClasses
      trxStatsClasses = case curr of
        Transactions TransactionStats -> selectedClasses
        _                             -> regularClasses
  in  do
        let miniMenuClasses = regularClasses <> " ml-5"
        let nanoMenuClasses = regularClasses <> " ml-10"
        H.a ! A.href "/" ! A.class_ (currPageIs Homepage) $ "Chewpaca Home"
        H.a ! A.href "/dashboard" ! A.class_ (currPageIs Dashboard) $ "Dashboard"
        H.a ! A.href "/users" ! A.class_ userSectionClasses $ "User List"
        H.a ! A.href "/users/search" ! A.class_ (currPageIs (Users SearchBar) <> miniMenuClasses) $ "User Search"
        case curr of
          Users (SearchResults t) -> do
            H.a ! A.href (toValue $ "/users/search/text?q=" <> t) ! A.class_ (selectedClasses <> nanoMenuClasses) $ toHtml $ T.take 20 t
          _ -> mempty
        H.a ! A.href "/users/geo" ! A.class_ (geoReportClasses <> miniMenuClasses) $ "Geo report"
        H.a ! A.href "/users/cardcreated" ! A.class_ (cardNotActiveClasses <> miniMenuClasses) $ "Card not activated list"
        H.a ! A.href "/users/underreview" ! A.class_ (cardUnderReviewClasses <> miniMenuClasses) $ "Under review list"
        case curr of
          Users (SpecificUser (username, UserID u)) -> do
            let nameToShow = T.take 20 $ fromMaybe (toText u) username
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#top") ! A.class_ (sepcificUserClasses <> miniMenuClasses) $ toHtml nameToShow
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#notes") ! A.class_ nanoMenuClasses $ "Notes"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#kycassessments") ! A.class_ nanoMenuClasses $ "KYC"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#quickstats") ! A.class_ nanoMenuClasses $ "Stats"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#tokens") ! A.class_ nanoMenuClasses $ "Tokens (Codes)"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#ledger") ! A.class_ nanoMenuClasses $ "Ledger"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#riskscores") ! A.class_ nanoMenuClasses $ "Risk scores"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#cards") ! A.class_ nanoMenuClasses $ "Cards"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#transactions") ! A.class_ nanoMenuClasses $ "Transactions"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#payments") ! A.class_ nanoMenuClasses $ "Payments"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#groups") ! A.class_ nanoMenuClasses $ "Groups"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#revisions") ! A.class_ nanoMenuClasses $ "Revisions"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#balances") ! A.class_ nanoMenuClasses $ "Balances"
            H.a ! A.href (toValue $ "/user/" <> toText u <> "#actions") ! A.class_ nanoMenuClasses $ "Actions"
          Users GeoStats -> do
            H.a ! A.href "/users/geo#allusers-state" ! A.class_ nanoMenuClasses $ "All users map"
            H.a ! A.href "/users/geo#allusers-cities" ! A.class_ nanoMenuClasses $ "All users by cities"
            H.a ! A.href "/users/geo#activecards-state" ! A.class_ nanoMenuClasses $ "Active cards map"
            H.a ! A.href "/users/geo#conversion-state" ! A.class_ nanoMenuClasses $ "Conversion map"
            H.a ! A.href "/users/geo#kycfail-state" ! A.class_ nanoMenuClasses $ "KYC Failed map"
          _ -> mempty
        H.a ! A.href "/ledger/journals" ! A.class_ (currPageIs LedgerJournal) $ "Ledger Journals"
        H.a ! A.href "/rewards/list" ! A.class_ (currPageIs Rewards) $ "Rewards"
        -- Group Section
        H.a ! A.href "/groups" ! A.class_ groupSectionClasses $ "Group List"
        H.a ! A.href "/group/new" ! A.class_ miniMenuClasses $ "New group"
        case curr of
          Groups (Just (GroupId g)) -> do
            H.a ! A.href (toValue $ "/group/" <> toText g <> "#top") ! A.class_ miniMenuClasses $ "Top of page"
            H.a ! A.href (toValue $ "/group/" <> toText g <> "#revisions") ! A.class_ nanoMenuClasses $ "Revisions"
            H.a ! A.href (toValue $ "/group/" <> toText g <> "#actions") ! A.class_ nanoMenuClasses $ "Actions"
          _ -> mempty
        -- Transaction Section
        H.a ! A.href "/transactions" ! A.class_ transactionSectionClasses $ "Transaction List"
        H.a ! A.href "/transactions/stats" ! A.class_ (trxStatsClasses <> miniMenuClasses) $ "Reports"
        case curr of
          Transactions (SpecificTransaction (TransactionId t, _)) -> do
            H.a ! A.href (toValue $ "/transaction/" <> toText t <> "#top") ! A.class_ miniMenuClasses $ "Top of page"
          _ -> mempty
        -- Payment section
        H.a ! A.href "/payments" ! A.class_ paymentSectionClasses $ "Payment List"
        case curr of
          Payments (Just (PaymentId p)) -> do
            H.a ! A.href (toValue $ "/payment/" <> toText p <> "#top") ! A.class_ miniMenuClasses $ "Top of page"
          _ -> mempty
        -- Debug section
        H.a ! A.href "/messages?offset=0" ! A.class_ regularClasses $ "Messages"
        H.a ! A.href "/messages/apto?offset=0" ! A.class_ miniMenuClasses $ "Apto Messagees"

-- inline brittany config for width
-- brittany-next-binding --columns 1500
appShell :: CurrentSection -> Html -> Html
appShell currPage shellContent = do
  let pageTitle :: Html = case currPage of
        Homepage                -> "Chewpaca"
        Users        MainList   -> "User List"
        Users        SearchBar  -> "User Search"
        Users (SearchResults t) -> toHtml $ "Search: " <> t
        Users        GeoStats   -> "User Geogrpahy Report"
        Users CardNotActive     -> "Card not activated list"
        Users UnderReviewList   -> "Under review list"
        Users (SpecificUser (name, UserID u)) -> toHtml $ fromMaybe (toText u) name
        Transactions MainTrxList -> "Transaction List"
        Transactions TransactionStats -> "Transaction Stats"
        Transactions (SpecificTransaction _) -> "Transaction"
        Payments     Nothing    -> "Payment list"
        Payments     (Just _)   -> "Payment"
        Groups       Nothing    -> "Group List"
        Groups       (Just _)   -> "Group"
        Dashboard               -> "Dashboard"
        LedgerJournal           -> "LedgerJournal"
        Rewards                 -> "Rewards"
        Reward RewardBoost {..} -> toHtml boostName
  H.html $ do
    H.head $ do
      H.title pageTitle
      H.meta ! A.charset "utf-8"
      H.link ! A.rel "stylesheet" ! A.href "https://unpkg.com/tailwindcss@^1.5.1/dist/tailwind.min.css"
      H.link ! A.rel "stylesheet" ! A.href "https://cdn.jsdelivr.net/npm/@tailwindcss/ui@0.6.0/dist/tailwind-ui.css"
      H.link ! A.rel "stylesheet" ! A.href "https://rsms.me/inter/inter.css"
      H.script ! A.src "https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.6.0/dist/alpine.min.js" $ mempty
      H.script ! A.src "https://cdn.jsdelivr.net/npm/lodash@4.17.20/lodash.min.js" $ mempty
      H.script ! A.src "https://cdn.amcharts.com/lib/version/4.10.2/core.js" $ mempty
      H.script ! A.src "https://cdn.amcharts.com/lib/version/4.10.2/charts.js" $ mempty
      H.script ! A.src "https://cdn.amcharts.com/lib/version/4.10.2/maps.js" $ mempty
      H.script ! A.src "https://cdn.amcharts.com/lib/version/4.10.2/themes/animated.js" $ mempty
      H.script ! A.src "https://cdn.amcharts.com/lib/version/geodata/4.1.17/usaLow.js" $ mempty
      H.script ! A.src "/charts.js" $ mempty
    H.body $ do
      H.div ! A.class_ "h-screen flex overflow-hidden bg-gray-100" $ do
        --  Off-canvas menu for mobile 
        H.div ! A.class_ "hidden" $ H.div ! A.class_ "fixed inset-0 flex z-40" $ do
          -- 
          --         Off-canvas menu overlay, show/hide based on off-canvas menu state.
          -- 
          --         Entering: "transition-opacity ease-linear duration-300"
          --           From: "opacity-0"
          --           To: "opacity-100"
          --         Leaving: "transition-opacity ease-linear duration-300"
          --           From: "opacity-100"
          --           To: "opacity-0"
          --       
          H.div ! A.class_ "fixed inset-0" $ do
            H.div ! A.class_ "absolute inset-0 bg-gray-600 opacity-75" $ mempty
          -- 
          --         Off-canvas menu, show/hide based on off-canvas menu state.
          -- 
          --         Entering: "transition ease-in-out duration-300 transform"
          --           From: "-translate-x-full"
          --           To: "translate-x-0"
          --         Leaving: "transition ease-in-out duration-300 transform"
          --           From: "translate-x-0"
          --           To: "-translate-x-full"
          --       
          H.div ! A.class_ "relative flex-1 flex flex-col max-w-xs w-full pt-5 pb-4 bg-indigo-800" $ do
            H.div ! A.class_ "absolute top-0 right-0 -mr-14 p-1" $ do
              H.button ! A.class_ "flex items-center justify-center h-12 w-12 rounded-full focus:outline-none focus:bg-gray-600" ! ariaLabel "Close sidebar" $ do
                S.svg ! SA.class_ "h-6 w-6 text-white" ! SA.stroke "currentColor" ! SA.fill "none" ! SA.viewbox "0 0 24 24" $ do
                  S.path ! SA.strokeLinecap "round" ! SA.strokeLinejoin "round" ! SA.strokeWidth "2" ! SA.d "M6 18L18 6M6 6l12 12"
            H.div ! A.class_ "flex-shrink-0 flex items-center px-4" $ do
              H.div ! A.class_ "h-8 w-auto" $ mempty
            H.div ! A.class_ "mt-5 flex-1 h-0 overflow-y-auto" $ do
              H.nav ! A.class_ "px-2 space-y-1" $ do
                sidebarHtml MobileSidebar currPage
          H.div ! A.class_ "flex-shrink-0 w-14" $ mempty --  Dummy element to force sidebar to shrink to fit close icon 
        --  Static sidebar for desktop 
        H.div ! A.class_ "hidden md:flex md:flex-shrink-0" $ H.div ! A.class_ "flex flex-col w-64" $ do
          --  Sidebar component, swap this element with another sidebar if you like 
          H.div ! A.class_ "flex flex-col flex-grow bg-indigo-800 pt-5 pb-4 overflow-y-auto" $ do
            H.div ! A.class_ "flex items-center flex-shrink-0 px-4" $ do
              H.div ! A.class_ "h-8 w-auto" $ mempty
            H.div ! A.class_ "mt-5 flex-1 flex flex-col" $ H.nav ! A.class_ "flex-1 px-2 bg-indigo-800 space-y-1" $ do
              sidebarHtml DesktopSidebar currPage
        H.div ! A.class_ "flex flex-col w-0 flex-1 overflow-hidden" $ do
          H.div ! A.class_ "relative z-10 flex-shrink-0 flex h-20 bg-white shadow" $ do
            H.button ! A.class_ "px-4 border-r border-gray-200 text-gray-500 focus:outline-none focus:bg-gray-100 focus:text-gray-600 md:hidden" ! ariaLabel "Open sidebar" $ do
              S.svg ! SA.class_ "h-6 w-6" ! SA.stroke "currentColor" ! SA.fill "none" ! SA.viewbox "0 0 24 24" $ do
                S.path ! SA.strokeLinecap "round" ! SA.strokeLinejoin "round" ! SA.strokeWidth "2" ! SA.d "M4 6h16M4 12h16M4 18h7"
            H.div ! A.class_ "flex-1 px-4 flex justify-between" $ do
              breadcrumbHtml currPage

          H.main ! A.class_ "flex-1 relative overflow-y-auto focus:outline-none" ! A.tabindex "0" $ do
            H.div ! A.class_ "max-w-7xl mx-auto px-4 py-4 sm:px-6 lg:px-8" $ do
              --  Replace with your content 
              shellContent
              --  /End replace 

