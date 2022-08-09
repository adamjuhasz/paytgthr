{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{-# Language RecordWildCards, QuasiQuotes #-}

module Chewpaca.Web.Users.GeoStats where

import           Chewpaca.Tailwind.Classes      ( dataTable
                                                , tableCellClasses
                                                , tableFirstCellClasses
                                                , tableHeaderCellClasses
                                                )
import           Data.Aeson                     ( encode
                                                , object
                                                , KeyValue((.=))
                                                )
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Data.List                      ( group
                                                , groupBy
                                                , sort
                                                , sortOn
                                                )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           Data.Ord                       ( Down(Down) )
import qualified Data.Text                     as T
import           Shared.Models.Card             ( CardStatus(CardActive) )
import           Shared.Models.User             ( UserModel(..)
                                                , UserState(..)
                                                )
import           Text.Blaze.Html5               ( (!)
                                                , Html
                                                , toHtml
                                                )
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.RawString.QQ              ( r )

-- inline brittany config for width
-- brittany-next-binding --columns 1000
renderGeoStats :: [UserModel] -> Html
renderGeoStats users = do
  let hasState               = filter (isJust . usrAddressState) users
      groupedByState         = sortOn (Data.Ord.Down . length) . group . sort $ fmap (\UserModel {..} -> "US-" <> fromJust usrAddressState) hasState
      convertedByState       = groupBy (\(t1, _) (t2, _) -> t1 == t2) . sortOn fst $ fmap (\UserModel {..} -> ("US-" <> fromJust usrAddressState, usrUserState == UserActive)) hasState
      conversionByState      = fmap (\state -> (fst (head state), length (filter snd state), length state)) convertedByState
      kycfailByState         = groupBy (\(t1, _) (t2, _) -> t1 == t2) . sortOn fst $ fmap (\UserModel {..} -> ("US-" <> fromJust usrAddressState, usrUserState == UserKYCDelay || usrUserState == UserUpdatedKYCDelay)) hasState
      kycfailByCountsByState = fmap (\state -> (fst (head state), length (filter snd state), length state)) kycfailByState
      kycfailByCity          = groupBy (\(t1, _) (t2, _) -> t1 == t2) . sortOn fst $ fmap (\UserModel {..} -> (fromJust usrAddressCity, usrUserState == UserKYCDelay || usrUserState == UserUpdatedKYCDelay)) hasState
      kycfailByCountsByCity  = fmap (\state -> (fst (head state), length (filter snd state), length state)) kycfailByCity
      activeByState          = groupBy (\(t1, _) (t2, _) -> t1 == t2) . sortOn fst $ fmap (\UserModel {..} -> ("US-" <> fromJust usrAddressState, usrAptoCardStatus == Just CardActive)) hasState
      activeCountsByState    = sortOn (\(_, _, a) -> a) $ fmap (\state -> (fst (head state), length (filter snd state), length state)) activeByState
      groupedByCity          = sortOn (Data.Ord.Down . length) . group . sort $ fmap (\UserModel {..} -> fromJust usrAddressCity) hasState
      jsonString             = decodeUtf8 . encode $ fmap (\state -> object ["id" .= head state, "value" .= length state]) groupedByState
      conversionJSON         = decodeUtf8 . encode $ fmap (\(state, trueFor, allUsers) -> object ["id" .= state, "value" .= (realToFrac trueFor / realToFrac allUsers * 100.0 :: Double)]) conversionByState
      kycJSON                = decodeUtf8 . encode $ fmap (\(state, trueFor, allUsers) -> object ["id" .= state, "value" .= (realToFrac trueFor / realToFrac allUsers * 100.0 :: Double)]) kycfailByCountsByState
      activeJSON             = decodeUtf8 . encode $ fmap (\(state, trueFor, allUsers) -> object ["id" .= state, "value" .= (realToFrac trueFor / realToFrac allUsers * 100.0 :: Double)]) activeCountsByState

  H.script $ toHtml $ "geoData = " <> jsonString <> ";"
  H.script $ toHtml $ "conversionData = " <> conversionJSON <> ";"
  H.script $ toHtml $ "kycFailedData = " <> kycJSON <> ";"
  H.script $ toHtml $ "activeData = " <> activeJSON <> ";"

  H.script $ do
    [r| 
    function createMap(divid, chartdata) {
      // Create map instance
      var chart = am4core.create(divid, am4maps.MapChart);

      // Set map definition
      chart.geodata = am4geodata_usaLow;

      // Set projection
      chart.projection = new am4maps.projections.AlbersUsa();

      // Create map polygon series
      var polygonSeries = chart.series.push(new am4maps.MapPolygonSeries());

      //Set min/max fill color for each area
      polygonSeries.heatRules.push({
        property: "fill",
        target: polygonSeries.mapPolygons.template,
        min: chart.colors.getIndex(1).brighten(1),
        max: chart.colors.getIndex(1).brighten(-0.3)
      });

      // Make map load polygon data (state shapes and names) from GeoJSON
      polygonSeries.useGeodata = true;

      // Set heatmap values for each state
      polygonSeries.data = chartdata;

      // Configure series tooltip
      var polygonTemplate = polygonSeries.mapPolygons.template;
      polygonTemplate.tooltipText = "{name}: {value}";
      polygonTemplate.nonScalingStroke = true;
      polygonTemplate.strokeWidth = 0.5;

      // Create hover state and set alternative fill color
      var hs = polygonTemplate.states.create("hover");
      hs.properties.fill = am4core.color("#3c5bdc");
    }

    am4core.ready(function() {
      // Themes begin
      am4core.useTheme(am4themes_animated);
      // Themes end
      createMap("allchartdiv", geoData);
      createMap("conversionchartdiv", conversionData);
      createMap("kycchartdiv", kycFailedData);
      createMap("activecardsdiv", activeData);
    });
    |]

  H.h3 ! A.id "allusers-state" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "All Users - by state (not adjusted by relative population)"
  H.div $ do
    H.div ! A.id "allchartdiv" ! A.style "width: 100%; height: 500px;" $ mempty

  H.h3 ! A.id "allusers-cities" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "All users - top cities"
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "City"
        H.th ! A.class_ tableHeaderCellClasses $ "Number"
    H.tbody $ do
      let createRow city = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                toHtml $ head city
              H.td ! A.class_ tableCellClasses $ do
                toHtml $ length city
      mapM_ createRow $ take 20 groupedByCity

  H.h3 ! A.id "activecards-state" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "% with activated cards map"
  H.div $ do
    H.div ! A.id "activecardsdiv" ! A.style "width: 100%; height: 500px;" $ mempty

  H.h3 ! A.id "conversion-state" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "% Conversion by state map"
  H.div $ do
    H.div ! A.id "conversionchartdiv" ! A.style "width: 100%; height: 500px;" $ mempty

  H.h3 ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "Conversion by state list"
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Percent"
        H.th ! A.class_ tableHeaderCellClasses $ "Converted"
        H.th ! A.class_ tableHeaderCellClasses $ "All users"
    H.tbody $ do
      let createRow (state, converted, allusers) = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                toHtml $ T.drop 3 state
              H.td ! A.class_ tableFirstCellClasses $ do
                H.span $ toHtml (floor (realToFrac converted / realToFrac allusers * 100.0 :: Double) :: Int)
                H.span "%"
              H.td ! A.class_ tableCellClasses $ do
                toHtml converted
              H.td ! A.class_ tableCellClasses $ do
                toHtml allusers
      mapM_ createRow conversionByState

  H.h3 ! A.id "kycfail-state" ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "Auto KYC Failed by state map"
  H.div $ do
    H.div ! A.id "kycchartdiv" ! A.style "width: 100%; height: 500px;" $ mempty

  H.h3 ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "Auto KYC Failed by state list"
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Percent"
        H.th ! A.class_ tableHeaderCellClasses $ "KYC Auto failed"
        H.th ! A.class_ tableHeaderCellClasses $ "All users"
    H.tbody $ do
      let createRow (state, converted, allusers) = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                toHtml $ T.drop 3 state
              H.td ! A.class_ tableFirstCellClasses $ do
                H.span $ toHtml (floor (realToFrac converted / realToFrac allusers * 100.0 :: Double) :: Int)
                H.span "%"
              H.td ! A.class_ tableCellClasses $ do
                toHtml converted
              H.td ! A.class_ tableCellClasses $ do
                toHtml allusers
      mapM_ createRow kycfailByCountsByState

  H.h3 ! A.class_ "mt-4 mb-5 text-lg leading-6 font-medium text-gray-900" $ do
    "Auto KYC Failed by City list"
  dataTable $ do
    H.thead $ do
      H.tr $ do
        H.th ! A.class_ tableHeaderCellClasses $ "State"
        H.th ! A.class_ tableHeaderCellClasses $ "Percent"
        H.th ! A.class_ tableHeaderCellClasses $ "KYC Auto failed"
        H.th ! A.class_ tableHeaderCellClasses $ "All users"
    H.tbody $ do
      let createRow (city, converted, allusers) = do
            H.tr ! A.class_ "bg-white" $ do
              H.td ! A.class_ tableFirstCellClasses $ do
                toHtml city
              H.td ! A.class_ tableFirstCellClasses $ do
                H.span $ toHtml (floor (realToFrac converted / realToFrac allusers * 100.0 :: Double) :: Int)
                H.span "%"
              H.td ! A.class_ tableCellClasses $ do
                toHtml converted
              H.td ! A.class_ tableCellClasses $ do
                toHtml allusers
      mapM_ createRow $ take 20 kycfailByCountsByCity
