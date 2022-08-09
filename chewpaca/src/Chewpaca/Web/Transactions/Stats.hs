{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# LANGUAGE RecordWildCards, QuasiQuotes #-}

module Chewpaca.Web.Transactions.Stats where

import           Chewpaca.Utils.Aggregation     ( groupedByGID
                                                , groupByMonth
                                                , mapSnd
                                                , median
                                                , onlyCompletedTrx
                                                , onlyDeclinedTrx
                                                , sortAndGroupBy
                                                )
import           Data.Aeson                     ( encode
                                                , object
                                                , KeyValue((.=))
                                                )
import           Data.List                      ( groupBy
                                                , sort
                                                , sortOn
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as TL
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Shared.Models.Currency         ( getMonetaryValue )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionState(..)
                                                , MerchantInfo(..)
                                                , MastercardMCC(..)
                                                , DeclineReason(..)
                                                )
import           Text.Blaze.Html5               ( Html
                                                , (!)
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.RawString.QQ              ( r )

getMCC :: Transaction -> Text
getMCC Transaction { trxMerchant = (Just CardMerchant { cmiMcc = MastercardMCC mcc }) }
  = mcc
getMCC _ = ""

getMCCDesc :: Transaction -> Text
getMCCDesc Transaction { trxMerchant = (Just CardMerchant { cmiMccDesc = t }) }
  = t
getMCCDesc _ = ""

groupIntoMCC :: [Transaction] -> [[(Text, Transaction)]]
groupIntoMCC =
  groupBy (\(a, _) (b, _) -> a == b) . sortOn fst . fmap (\t -> (getMCC t, t))

mccToCountJSON :: [[(Text, Transaction)]] -> TL.Text
mccToCountJSON = decodeUtf8 . encode . fmap
  (\elems -> object
    [ "category" .= fst (head elems)
    , "value" .= length elems
    , "name" .= getMCCDesc (snd $ head elems)
    ]
  )

mccToSumJSON :: [[(Text, Transaction)]] -> TL.Text
mccToSumJSON =
  decodeUtf8
    . encode
    . fmap
        (\(categ, desc, total :: Double) ->
          object ["category" .= categ, "value" .= total, "name" .= desc]
        )
    . filter (\(_, _, total) -> total > 0)
    . fmap
        (\elems ->
          ( fst (head elems)
          , getMCCDesc (snd $ head elems)
          , sum $ fmap
            (\(_, Transaction {..}) ->
              fromRational (getMonetaryValue trxDisplayAmount)
            )
            elems
          )
        )

mccToMedianJSON :: [[(Text, Transaction)]] -> TL.Text
mccToMedianJSON =
  decodeUtf8
    . encode
    . fmap
        (\(categ, desc, total :: Double) ->
          object ["category" .= categ, "value" .= total, "name" .= desc]
        )
    . filter (\(_, _, total) -> total > 0)
    . fmap
        (\elems ->
          ( fst (head elems)
          , getMCCDesc (snd $ head elems)
          , median . sort $ fmap
            (\(_, Transaction {..}) ->
              fromRational (getMonetaryValue trxDisplayAmount)
            )
            elems
          )
        )

showDeclineReason :: TransactionState -> String
showDeclineReason (TrxDeclined (LowBalance _)) = "LowBalance"
showDeclineReason (TrxDeclined (BalanceCheckFail _)) = "BalanceCheckFail"
showDeclineReason (TrxDeclined (ExceedMaxTrxAmount _)) = "ExceedMaxTrxAmount"
showDeclineReason (TrxDeclined (UserNotFound _)) = "UserNotFound"
showDeclineReason (TrxDeclined (PaymentUnlinked _)) = "PaymentUnlinked"
showDeclineReason (TrxDeclined (UserNotActive _)) = "UserNotActive"
showDeclineReason (TrxDeclined (Unknown _)) = "Unknown"
showDeclineReason (TrxDeclined dr) = show dr
showDeclineReason _ = error "not declined"

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderTrxStats :: [Transaction] -> Html
renderTrxStats trxs = do
  let completedTrx         = onlyCompletedTrx trxs
  let declinedTrx          = onlyDeclinedTrx trxs

  let completedMCCJSON     = mccToCountJSON $ groupIntoMCC completedTrx
  let compSummedMCCJSON    = mccToSumJSON $ groupIntoMCC completedTrx
  let completedMedianByMCC = mccToMedianJSON $ groupIntoMCC completedTrx
  let declinedMCCJSON      = mccToCountJSON $ groupIntoMCC declinedTrx

  let countNumber :: [(a, [b])] -> [(a, Int)]
      countNumber = mapSnd length

  let sumNumber :: [(a, [Transaction])] -> [(a, Int)]
      sumNumber = mapSnd (sum . fmap (ceiling . getMonetaryValue . trxDisplayAmount))

  let reasonForDecline = countNumber $ sortAndGroupBy (\Transaction { trxState = a } -> showDeclineReason a) declinedTrx

  let countByGroup     = mapSnd (length . groupedByGID)

  let trxReport f = f . groupByMonth trxPurchasedAt

  H.script $ toHtml $ "compMccData = " <> completedMCCJSON <> ";"
  H.script $ toHtml $ "compMccDataSum = " <> compSummedMCCJSON <> ";"
  H.script $ toHtml $ "completedMedianByMCC = " <> completedMedianByMCC <> ";"
  H.script $ toHtml $ "declMccData = " <> declinedMCCJSON <> ";"
  H.script $ toHtml $ "reasonForDecline = '" <> show reasonForDecline <> "';"

  H.script $ toHtml $ "totalTrxByMonth = " <> (decodeUtf8 . encode . fmap (\(month, len) -> object ["month" .= month, "len" .= len]) $ trxReport countNumber trxs)
  H.script $ toHtml $ "totalActiveGroupByMonth = " <> (decodeUtf8 . encode . fmap (\(month, len) -> object ["month" .= month, "len" .= len]) $ trxReport countByGroup trxs)

  H.script $ toHtml $ "sumTrxByMonth = " <> (decodeUtf8 . encode . fmap (\(month, len) -> object ["month" .= month, "len" .= len]) $ trxReport sumNumber trxs)
  H.script $ toHtml $ "sumCompletedTrxByMonth = " <> (decodeUtf8 . encode . fmap (\(month, len) -> object ["month" .= month, "len" .= len]) $ trxReport sumNumber completedTrx)

  H.script $ toHtml $ "completedTrxByMonth = " <> (decodeUtf8 . encode . fmap (\(month, len) -> object ["month" .= month, "len" .= len]) $ trxReport countNumber completedTrx)
  H.script $ toHtml $ "completedActiveGroupByMonth = " <> (decodeUtf8 . encode . fmap (\(month, len) -> object ["month" .= month, "len" .= len]) $ trxReport countByGroup completedTrx)

  H.script $ do
    [r|
    am4core.ready(function() {

      // Themes begin
      // am4core.useTheme(am4themes_animated);
      // Themes end

      createRadarChart("completedChart", compMccData);
      createRadarChart("compSumChart", compMccDataSum);
      createRadarChart("declinedChart", declMccData);
      createRadarChart("completedMedianByMCC", completedMedianByMCC);

      var colors = new am4core.ColorSet();
      createBarChart("totalTrxByMonth", "Total Trx by month", totalTrxByMonth.map(elem => ({date: new Date(elem.month), month: elem.month, value: elem.len})), colors.getIndex(0));
      createBarChart("totalActiveGroupByMonth", "Spending Groups by month", totalActiveGroupByMonth.map(elem => ({date: new Date(elem.month), month: elem.month, value: elem.len})), colors.getIndex(0));

      createBarChart("sumTrxByMonth", "", sumTrxByMonth.map(elem => ({date: new Date(elem.month), month: elem.month, value: elem.len})), colors.getIndex(0));
      createBarChart("sumCompletedTrxByMonth", "", sumCompletedTrxByMonth.map(elem => ({date: new Date(elem.month), month: elem.month, value: elem.len})), colors.getIndex(0));

      createBarChart("completedTrxByMonth", "Total Trx by month", completedTrxByMonth.map(elem => ({date: new Date(elem.month), month: elem.month, value: elem.len})), colors.getIndex(0));
      createBarChart("completedActiveGroupByMonth", "Spending Groups by month", completedActiveGroupByMonth.map(elem => ({date: new Date(elem.month), month: elem.month, value: elem.len})), colors.getIndex(0));

    }); // end am4core.ready()
    |]

  -- ALL Trx
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total Transactions"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "totalTrxByMonth" ! A.style "height: 150px;" $ mempty
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "# of Groups Making Transactions"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "totalActiveGroupByMonth" ! A.style "height: 150px;" $ mempty

  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total Transaction amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "sumTrxByMonth" ! A.style "height: 150px;" $ mempty
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Completed Transaction amount"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "sumCompletedTrxByMonth" ! A.style "height: 150px;" $ mempty

  -- Compeleted TRX
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Completed Transactions"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "completedTrxByMonth" ! A.style "height: 150px;" $ mempty
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "# of Groups Making Completed Trx"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "completedActiveGroupByMonth" ! A.style "height: 150px;" $ mempty

  -- Compeleted TRX by MCC
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Mediun Transaction amount by MCC "
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "completedMedianByMCC" ! A.style "height: 500px;" $ mempty
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Total $ Completed Transactions by MCC"
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "compSumChart" ! A.style "height: 500px;" $ mempty

  -- Declined
  H.div ! A.class_ "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2" $ do
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Count Completed Transactions by MCC "
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "completedChart" ! A.style "height: 500px;" $ mempty
    H.div ! A.class_ "bg-white overflow-hidden shadow rounded-lg" $ do
      H.div ! A.class_ "px-4 py-5 sm:p-6" $ do
        H.dl $ do
          H.dt ! A.class_ "text-sm leading-5 font-medium text-gray-500 truncate" $ do
            "Count Declined Transactions by MCC "
          H.dd ! A.class_ "mt-1 text-3xl leading-9 font-semibold text-gray-900" $ do
            H.div ! A.id "declinedChart" ! A.style "height: 500px;" $ mempty
