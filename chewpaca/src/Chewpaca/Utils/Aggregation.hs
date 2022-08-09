{-# LANGUAGE RecordWildCards #-}

module Chewpaca.Utils.Aggregation where

import           Data.Aeson                     ( KeyValue((.=))
                                                , ToJSON
                                                , encode
                                                , object
                                                )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Coerce                    ( coerce )
import           Data.List                      ( groupBy
                                                , sort
                                                , sortOn
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Data.Time.Clock                ( UTCTime )
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import           Data.UUID                      ( toText )
import           Shared.Models.Group            ( GroupId(..) )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionState(..)
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                , toValue
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f xys = [ (x, f y) | (x, y) <- xys ]

nestedMapSnd :: (x -> y) -> [(a, [(b, x)])] -> [(a, [(b, y)])]
nestedMapSnd = mapSnd . mapSnd

-- >>> sortAndGroupBy fst [("a", 14.0), ("b", 3.0),("a", 2.0)]
-- [("a",[("a",14.0),("a",2.0)]),("b",[("b",3.0)])]
sortAndGroupBy :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortAndGroupBy f =
  fmap (\xs -> (f (head xs), xs)) <$> groupBy (\a b -> f a == f b) . sortOn f

-- >>> median [1]
-- 1.0
-- >>> median [2,1]
-- 1.5
-- >>> median [2,1,3]
-- 2.0
-- >>> median [2,1,3,4]
-- 2.5
median :: (Ord a, Fractional a) => [a] -> a
median [] = error "not on empty"
median xs | odd len   = sorted !! mid
          | even len  = evenMedian
          | otherwise = error "otherwise in median"
 where
  sorted     = sort xs
  len        = length xs
  mid        = len `div` 2
  evenMedian = (sorted !! (mid - 1) + sorted !! mid) / 2

-- >>> mean [1]
-- 1.0

-- >>> mean [1,2]
-- 1.5

-- >>> mean [1.5,2.5,3.5]
-- 2.5
mean :: Fractional a => [a] -> a
mean [] = error "not on empty"
mean xs = sum xs / realToFrac (length xs)

-- >>> mode [1,2,3]
-- 3
-- >>> mode [4,1,2,3]
-- 4
-- >>> mode [4,1,5,2,3]
-- 5
mode :: Ord a => [a] -> a
mode []       = error "not on empty"
mode [x     ] = x
mode (x : xs) = if x > remainingsMode then x else remainingsMode
  where remainingsMode = mode xs

isDeclined :: TransactionState -> Bool
isDeclined (TrxDeclined _) = True
isDeclined _               = False

convertToMonthOnly :: UTCTime -> String
convertToMonthOnly = formatTime defaultTimeLocale "%m"

type MonthString = String
convertToMonth :: UTCTime -> MonthString
convertToMonth = formatTime defaultTimeLocale "%Y-%m"

groupByMonth :: (a -> UTCTime) -> [a] -> [(MonthString, [a])]
groupByMonth f = sortAndGroupBy (convertToMonth . f)

type WeekString = String
groupByWeek :: (a -> UTCTime) -> [a] -> [(WeekString, [a])]
groupByWeek f = sortAndGroupBy (formatTime defaultTimeLocale "%Y-%U" . f)

type DayString = String
groupByDay :: (a -> UTCTime) -> [a] -> [(DayString, [a])]
groupByDay f = sortAndGroupBy (formatTime defaultTimeLocale "%Y-%m-%d" . f)

type GroupIdText = Text
groupedByGID :: [Transaction] -> [(Maybe GroupIdText, [Transaction])]
groupedByGID = sortAndGroupBy
  (\Transaction { trxGroupId = gid } -> toText . coerce . fst <$> gid)

toChartJSON :: (ToJSON a, ToJSON b) => Text -> (Text, [(a, b)]) -> Html
toChartJSON keyName (chartName, xs) = do
  let chartVals = toStrict . decodeUtf8 . encode $ fmap
        (\(idx, val) -> object [keyName .= idx, "value" .= val])
        xs
  H.script $ toHtml $ chartName <> " = " <> chartVals <> ";"

onlyCompletedTrx :: [Transaction] -> [Transaction]
onlyCompletedTrx = filter
  (\Transaction {..} -> not (isDeclined trxState) && trxDisplayAmount /= 0)

onlyDeclinedTrx :: [Transaction] -> [Transaction]
onlyDeclinedTrx = filter (\Transaction {..} -> isDeclined trxState)

toChartDisplay :: (ToJSON a, ToJSON b) => Text -> (Text, [(a, b)]) -> Html
toChartDisplay keyName (chartName, xs) = do
  toChartJSON keyName (chartName, xs)
  H.div ! A.id (toValue chartName) ! A.style "height: 150px;" $ mempty

debugJSON :: ToJSON a => ByteString -> a -> Html
debugJSON name vals =
  H.script $ toHtml $ decodeUtf8 $ name <> " = " <> encode vals <> ";"
