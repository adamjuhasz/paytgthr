{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}


module Model.CardSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile
                                                , filter
                                                )
import           Test.Hspec
import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy           ( readFile )

import           APIApto.Model.Webhook
import           Shared.Models.Apto.Base
import           Shared.Models.Apto.Card
import           Shared.Utils
import           Shared.Models.Card

spec :: Spec
spec = parallel $ do
  describe "CardUpdate" $ do
    it "Created" $ do
      let msg = CardUpdate
            { crdId           = AptoCardId "crd_62d4c88d3f8ae49a"
            , crdCardholderId = AptoCardholderId "crdhldr_a"
            , crdDesign       = UnknownDesign "blue"
            , crdStatus       = CardCreated
            , crdLastFour     = Just $ CardLastFour "7204"
            , crdCreated      = Just $ stringToTime "2019-10-04T20:20:11+00:00"
            , crdActivated    = Nothing
            , crdProgram      = "Paytogether"
            , crdEvent        = StatusUpdate
            }
      someJson <- readFile "test/json/apto-card-created.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Activated" $ do
      let msg = CardUpdate
            { crdId           = AptoCardId "crd_504417abf2582de3"
            , crdCardholderId = AptoCardholderId "crdhldr_a"
            , crdDesign       = UnknownDesign "blue"
            , crdStatus       = CardActive
            , crdLastFour     = Just $ CardLastFour "1631"
            , crdCreated      = Just $ stringToTime "2019-10-04T20:20:12+00:00"
            , crdActivated    = Just $ stringToTime "2019-10-04T20:21:52+00:00"
            , crdProgram      = "Paytogether"
            , crdEvent        = StatusUpdate
            }
      someJson <- readFile "test/json/apto-card-activated.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Pin Updated" $ do
      let msg = CardUpdate
            { crdId           = AptoCardId "crd_504417abf2582de3"
            , crdCardholderId = AptoCardholderId "crdhldr_a"
            , crdDesign       = UnknownDesign "blue"
            , crdStatus       = CardActive
            , crdLastFour     = Just $ CardLastFour "1631"
            , crdCreated      = Just $ stringToTime "2019-10-04T20:20:12+00:00"
            , crdActivated    = Just $ stringToTime "2019-10-04T20:21:52+00:00"
            , crdProgram      = "Paytogether"
            , crdEvent        = PinUpdated
            }
      someJson <- readFile "test/json/apto-card-pinupdated.json"
      A.eitherDecode someJson `shouldBe` Right msg

    it "Closed" $ do
      let msg = CardUpdate
            { crdId           = AptoCardId "crd_3ec5df52a998a6b3"
            , crdCardholderId = AptoCardholderId "crdhldr_a"
            , crdDesign       = YellowToPink
            , crdStatus       = CardClosed
            , crdLastFour     = Nothing
            , crdCreated      = Nothing
            , crdActivated    = Nothing
            , crdProgram      = "Paytogether"
            , crdEvent        = StatusUpdate
            }
      someJson <- readFile "test/json/apto-card-closed.json"
      A.eitherDecode someJson `shouldBe` Right msg
