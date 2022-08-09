{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards #-}

module Chewpaca.Web.Dashboard where

import           Chewpaca.Utils.Aggregation     ( convertToMonthOnly
                                                , groupByDay
                                                , groupByMonth
                                                , groupByWeek
                                                , groupedByGID
                                                , mapSnd
                                                , mean
                                                , median
                                                , mode
                                                , nestedMapSnd
                                                , onlyCompletedTrx
                                                , sortAndGroupBy
                                                , toChartDisplay
                                                , toChartJSON
                                                )
import           Data.List                      ( group
                                                , sort
                                                , sortOn
                                                )
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , listToMaybe
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Time.Clock                ( diffUTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Shared.Models.Currency         ( MValue
                                                , getMonetaryValue
                                                )
import           Shared.Models.Transaction      ( Transaction(..) )
import           Shared.Models.User             ( UserID(..)
                                                , UserModel(..)
                                                , UserState(..)
                                                )
import           Shared.Utils                   ( stringToTime )
import           Text.Blaze.Html5               ( (!)
                                                , AttributeValue
                                                , Html
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.Read                      ( readMaybe )

type BirthYear = String
type GenerationString = String
binAge :: Maybe BirthYear -> GenerationString
binAge Nothing = error "Can't be Nothing"
binAge (Just years) | yearInt < 1946                   = "5 Silent Gen"
                    | 1946 <= yearInt, yearInt <= 1964 = "4 Boomer"
                    | 1965 <= yearInt, yearInt <= 1979 = "3 Gen X"
                    | 1980 <= yearInt, yearInt <= 1994 = "2 Millenial"
                    | 1995 <= yearInt                  = "1 Gen Z"
                    | otherwise = error $ "What is this year " <> show yearInt
 where
  yearInt :: Int =
    fromMaybe (error $ "years can't not be Int: " <> years) (readMaybe years)

drawCharts :: [(Text, a)] -> [(Text, [(Text, b)])] -> Html
drawCharts bars stacks = do
  let js :: Text =
        "am4core.ready(function() {\n"
          <> T.concat
               (fmap
                 (\(name, _) ->
                   "createBarChart('" <> name <> "', '', " <> name <> ");\n"
                 )
                 bars
               )
          <> T.concat
               (fmap
                 (\(name, serieses) ->
                   "createLayeredChart('"
                     <> name
                     <> "', ["
                     <> T.intercalate
                          ", "
                          (fmap (\(t, _) -> "['" <> t <> "', " <> t <> "]")
                                serieses
                          )
                     <> "]);\n"
                 )
                 stacks
               )
          <> "});"
  H.script $ toHtml js

type Days = Int

createTrxDB :: [Transaction] -> [(UserID, [Transaction])]
createTrxDB = mapSnd (fmap snd) . sortAndGroupBy fst . concatMap
  (\t@Transaction {..} -> (, t) . fst <$> trxSplitAmounts)

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderDashboard :: [Transaction] -> [UserModel] -> Html
renderDashboard trxs users = do
  let roundToDollar :: MValue -> Int
      roundToDollar = round
  let _userDB                         = fmap (\u -> (usrUserID u, u)) users

  let completedTrxsByMonth            = groupByMonth trxPurchasedAt $ onlyCompletedTrx trxs
  let completedtrxsByMonthAndGroup    = mapSnd groupedByGID completedTrxsByMonth
  let completedtrxsByMonthAndGroupSum = nestedMapSnd (sum . fmap (getMonetaryValue . trxDisplayAmount)) completedtrxsByMonthAndGroup
  let completedtrxsByMonthAndGroupLen = nestedMapSnd length completedtrxsByMonthAndGroup

  let completedtrxsByMonthSumXXX f = mapSnd (roundToDollar . f . fmap snd) completedtrxsByMonthAndGroupSum
  let completedtrxsByMonthMedianSum = ("completedtrxsByMonthMedianSum", completedtrxsByMonthSumXXX median)
  let completedtrxsByMonthMeanSum   = ("completedtrxsByMonthMeanSum", completedtrxsByMonthSumXXX mean)
  let completedtrxsByMonthModeSum   = ("completedtrxsByMonthModeSum", completedtrxsByMonthSumXXX mode)

  let completedtrxsByMonthLenXXX f = mapSnd (round . f . fmap ((realToFrac :: Int -> Double) . snd)) completedtrxsByMonthAndGroupLen
  let completedtrxsByMonthMedianLen = ("completedtrxsByMonthMedianLen", completedtrxsByMonthLenXXX median)
  let completedtrxsByMonthMeanLen = ("completedtrxsByMonthMeanLen", completedtrxsByMonthLenXXX mean)
  let completedtrxsByMonthModeLen = ("completedtrxsByMonthModeLen", completedtrxsByMonthLenXXX mode)

  let signedUpByWeek = ("signedUpByWeek", mapSnd length $ groupByWeek usrCreatedOn users)
  let signedUpByMonth = ("signedUpByMonth", mapSnd length $ groupByMonth usrCreatedOn users)
  let signedUpByDay = ("signedUpByDay", mapSnd length $ groupByDay usrCreatedOn users)

  let usersByCreatedOnMonth         = groupByMonth usrCreatedOn users

  let ageOfUsers = mapSnd binAge . filter (isJust . snd) $ fmap (\UserModel {..} -> (usrUserState, formatTime defaultTimeLocale "%Y" <$> usrDOB)) users
  let agesOfAllUsers = ("agesOfAllUsers", mapSnd length . sortAndGroupBy id . fmap snd $ ageOfUsers)
  let agesOfCompletedUsers = ("agesOfCompletedUsers", mapSnd length . sortAndGroupBy id . fmap snd . filter ((== UserActive) . fst) $ ageOfUsers)

  let trxForUsers = mapSnd (fmap snd) . sortAndGroupBy fst . concatMap (\t@Transaction {..} -> fmap (\(usr, _) -> (usr, t)) trxSplitAmounts) . sortOn trxPurchasedAt . filter (\Transaction {..} -> trxDisplayAmount /= 0) $ trxs
  let usersWithTheirTrxs = nestedMapSnd fromJust . mapSnd (filter (isJust . snd)) . mapSnd (fmap (\u@UserModel {..} -> (u, listToMaybe =<< sortOn trxPurchasedAt <$> lookup usrUserID trxForUsers))) $ usersByCreatedOnMonth

  let medianOfDays :: [Days] -> Int
      medianOfDays [] = 0
      medianOfDays xs = round . median . fmap toRational $ xs
  let medianDaysToFirstTrx = ("medianDaysToFirstTrx", mapSnd medianOfDays . mapSnd (fmap (\(UserModel {..}, Transaction {..}) -> floor . (/ 86400) $ diffUTCTime trxPurchasedAt usrCreatedOn)) $ usersWithTheirTrxs)

  let timeToCardActive = ("timeToCardActive", mapSnd medianOfDays . mapSnd (fmap (\UserModel {..} -> floor . (/ 86400) $ diffUTCTime (fromJust usrCardActivatedOn) (fromJust usrCardCreatedOn))) . mapSnd (filter (\u -> isJust (usrCardCreatedOn u) && isJust (usrCardActivatedOn u))) $ usersByCreatedOnMonth)

  -- let firstTrxIsComplete        = mapSnd length . mapSnd (filter (\(_, Transaction {..}) -> not $ isDeclined trxState)) $ usersWithTheirTrxs
  -- let firstTrxIsDeclined        = mapSnd length . mapSnd (filter (\(_, Transaction {..}) -> isDeclined trxState)) $ usersWithTheirTrxs

  toChartJSON "bin" agesOfAllUsers
  toChartJSON "bin" agesOfCompletedUsers

  drawCharts [completedtrxsByMonthMedianSum, completedtrxsByMonthMeanSum, completedtrxsByMonthModeSum, completedtrxsByMonthMedianLen, completedtrxsByMonthMeanLen, completedtrxsByMonthModeLen, medianDaysToFirstTrx, timeToCardActive, signedUpByDay, signedUpByWeek, signedUpByMonth] [("agestacked", [agesOfAllUsers, agesOfCompletedUsers])]

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-3" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Mean total $ spent this month per group"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" completedtrxsByMonthMeanSum
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Median total $ spent this month per group"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" completedtrxsByMonthMedianSum
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Mode total $ spent this month per group"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" completedtrxsByMonthModeSum

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-3" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Mean number of trx per group this month"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" completedtrxsByMonthMeanLen
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Median number of trx per group this month"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" completedtrxsByMonthMedianLen
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Mode number of trx per group this month"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" completedtrxsByMonthModeLen

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Median days to first trx from user created"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" medianDaysToFirstTrx
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Median days from card created to activated"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" timeToCardActive

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-1" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "User registrations per day"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" signedUpByDay

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-1" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "User registrations per week"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" signedUpByWeek

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-1" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "User registrations per month"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            toChartDisplay "month" signedUpByMonth

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-1" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Age bins"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "agestacked" ! A.style "height: 250px;" $ mempty

  H.h1 ! A.class_ "mt-5" $ do
    "All"
  drawRetentionChurn trxs

  H.h1 ! A.class_ "mt-5" $ do
    "Completed"
  drawRetentionChurn $ onlyCompletedTrx trxs

intToMon :: Int -> Html
intToMon 1  = "Jan"
intToMon 2  = "Feb"
intToMon 3  = "Mar"
intToMon 4  = "Apr"
intToMon 5  = "May"
intToMon 6  = "Jun"
intToMon 7  = "Jul"
intToMon 8  = "Aug"
intToMon 9  = "Sep"
intToMon 10 = "Oct"
intToMon 11 = "Nov"
intToMon 12 = "Dec"
intToMon x  = toHtml x

colorTdBg :: (Int, Int) -> AttributeValue
colorTdBg (a, b) | (b - a) == 0 = "bg-purple-900 text-white "
                 | (b - a) == 1 = "bg-purple-800 text-white "
                 | (b - a) == 2 = "bg-purple-700 text-white "
                 | (b - a) == 3 = "bg-purple-600 text-white "
                 | (b - a) == 4 = "bg-purple-500 text-white "
                 | (b - a) == 5 = "bg-purple-400 text-black "
                 | (b - a) == 6 = "bg-purple-300 text-black "
                 | (b - a) == 7 = "bg-purple-200 text-black "
                 | otherwise    = "bg-purple-100 text-black "

-- inline brittany config for width
-- brittany-next-binding --columns 1000
drawRetentionChurn :: [Transaction] -> Html
drawRetentionChurn trxs = do
  let trxDb = createTrxDB . filter (\Transaction {..} -> trxPurchasedAt >= stringToTime "2020-01-01T00:00:00+00:00") $ trxs
  let trxDBPurchasedAt = mapSnd (\ts -> [read (head ts) :: Int .. read (last ts)]) . mapSnd (sort . fmap (\Transaction {..} -> convertToMonthOnly trxPurchasedAt)) $ trxDb
  let purchaseCohorts = fmap (\x -> (head x, length x)) . group . sort . concatMap (\(_, ts) -> (head ts, ) <$> ts) $ trxDBPurchasedAt

  let maxMonth         = 15

  H.h3 ! A.class_ "mt-5" $ do
    "Retention - over the year"
  H.div ! A.class_ "flex flex-col " $ do
    H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
      H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
        H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
          H.table ! A.class_ "min-w-full" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  mempty
                mapM_
                  (\(xs :: Int) -> do
                    H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      toHtml xs <> " mo"
                  )
                  [0 .. maxMonth - 1]
            H.tbody $ do
              mapM_
                (\(month :: Int) -> do
                  H.tr ! A.class_ "bg-gray-50" $ do
                    H.td ! A.class_ "px-6 py-3 bg-gray-50 align-middle text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      intToMon month
                    mapM_
                      (\c -> do
                        case (lookup (month, month) purchaseCohorts, lookup (month, c) purchaseCohorts) of
                          (Just z, Just v) -> do
                            H.td ! A.class_ (colorTdBg (month, c) <> "px-6 py-4 align-middle text-center whitespace-no-wrap text-sm leading-5") $ do
                              H.span $ do
                                let percent :: Double = fromRational (toRational v / toRational z)
                                let rounded :: Int    = round $ percent * 100
                                toHtml rounded <> "%"
                          (_, _) -> H.td ! A.class_ "bg-gray-50" $ mempty
                      )
                      [month .. maxMonth]
                )
                [1 .. maxMonth]

  H.h3 ! A.class_ "mt-5" $ do
    "Churn - month to month"
  H.div ! A.class_ "flex flex-col " $ do
    H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
      H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
        H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
          H.table ! A.class_ "min-w-full" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  mempty
                mapM_
                  (\(xs :: Int) -> do
                    H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      toHtml xs <> " mo"
                  )
                  [0 .. maxMonth - 1]
            H.tbody $ do
              mapM_
                (\(month :: Int) -> do
                  H.tr ! A.class_ "bg-gray-50" $ do
                    H.td ! A.class_ "px-6 py-3 bg-gray-50 align-middle text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      intToMon month
                    mapM_
                      (\c -> do
                        case (lookup (month, c - 1) purchaseCohorts, lookup (month, c) purchaseCohorts) of
                          (Nothing, Just _) -> H.td ! A.class_ (colorTdBg (month, c) <> "px-6 py-4 align-middle text-center whitespace-no-wrap text-sm leading-5") $ do
                            "0%"
                          (Just z, Just v) -> do
                            H.td ! A.class_ (colorTdBg (month, c) <> "px-6 py-4 align-middle text-center whitespace-no-wrap text-sm leading-5") $ do
                              H.span $ do
                                let percent :: Double = fromRational (toRational (z - v) / toRational z)
                                let rounded :: Int    = round $ percent * 100
                                toHtml rounded <> "%"
                          (_, _) -> H.td ! A.class_ "bg-gray-50" $ mempty
                      )
                      [month .. maxMonth]
                )
                [1 .. maxMonth]

  H.h3 ! A.class_ "mt-5" $ do
    "Retention - month to month"
  H.div ! A.class_ "flex flex-col " $ do
    H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
      H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
        H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
          H.table ! A.class_ "min-w-full" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  mempty
                mapM_
                  (\(xs :: Int) -> do
                    H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      toHtml xs <> " mo"
                  )
                  [0 .. maxMonth - 1]
            H.tbody $ do
              mapM_
                (\(month :: Int) -> do
                  H.tr ! A.class_ "bg-gray-50" $ do
                    H.td ! A.class_ "px-6 py-3 bg-gray-50 align-middle text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      intToMon month
                    mapM_
                      (\c -> do
                        case (lookup (month, c - 1) purchaseCohorts, lookup (month, c) purchaseCohorts) of
                          (Nothing, Just _) -> H.td ! A.class_ (colorTdBg (month, c) <> "px-6 py-4 align-middle text-center whitespace-no-wrap text-sm leading-5") $ do
                            "100%"
                          (Just z, Just v) -> do
                            H.td ! A.class_ (colorTdBg (month, c) <> "px-6 py-4 align-middle text-center whitespace-no-wrap text-sm leading-5") $ do
                              H.span $ do
                                let percent :: Double = fromRational (toRational v / toRational z)
                                let rounded :: Int    = round $ percent * 100
                                toHtml rounded <> "%"
                          (_, _) -> H.td ! A.class_ "bg-gray-50" $ mempty
                      )
                      [month .. maxMonth]
                )
                [1 .. maxMonth]

  let countMonth = maxMonth + 1
  H.h3 ! A.class_ "mt-5" $ do
    "Count"
  H.div ! A.class_ "flex flex-col " $ do
    H.div ! A.class_ "-my-2 overflow-x-auto sm:-mx-6 lg:-mx-8" $ do
      H.div ! A.class_ "py-2 align-middle inline-block min-w-full sm:px-6 lg:px-8" $ do
        H.div ! A.class_ "shadow overflow-hidden border-b border-gray-200 sm:rounded-lg" $ do
          H.table ! A.class_ "min-w-full" $ do
            H.thead $ do
              H.tr $ do
                H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                  mempty
                mapM_
                  (\(xs :: Int) -> do
                    H.th ! A.class_ "px-6 py-3 bg-gray-50 text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      toHtml xs <> " mo"
                  )
                  [0 .. maxMonth - 1]
            H.tbody $ do
              mapM_
                (\(month :: Int) -> do
                  H.tr ! A.class_ "bg-gray-50" $ do
                    H.td ! A.class_ "px-6 py-3 bg-gray-50 align-middle text-center text-xs leading-4 font-medium text-gray-500 uppercase tracking-wider" $ do
                      intToMon month
                    mapM_
                      (\c -> do
                        case lookup (month, c) purchaseCohorts of
                          Just v -> do
                            H.td ! A.class_ "text-gray-500 px-6 py-4 align-middle text-center whitespace-no-wrap text-sm leading-5" $ do
                              H.span $ do
                                toHtml v
                          _ -> H.td ! A.class_ "bg-gray-50" $ mempty
                      )
                      [month .. countMonth]
                )
                [1 .. countMonth]
