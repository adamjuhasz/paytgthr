{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}


module Shared.Models.Apto.CardSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile
                                                , filter
                                                )
import           Test.Hspec
import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy           ( readFile )

import           Shared.Models.Apto.Base
import           Shared.Models.Apto.Card
import           Shared.Utils
import           Shared.Models.Card

spec :: Spec
spec = parallel $ do
  describe "AptoCardResponse" $ do
    it "decodes json" $ do
      let
        card1 = AptoCard
          { acdxId           = AptoCardId "crd_2fc40d4d6a0771eb"
          , acdxProgram      = "paytogether"
          , acdxDesign       = UnknownDesign "blue"
          , acdxLastFour     = CardLastFour "1822"
          , acdxStatus       = CardCreated
          , acdxActivatedAt  = Nothing
          , acdxCreatedAt    = stringToTime "2019-09-13T23:02:27+00:00"
          , acdxCardholderId = AptoCardholderId "crdhldr_a"
          , acdxDDANumber    = Nothing
          , acdxABARouting   = Nothing
          }
        card2 = AptoCard
          { acdxId           = AptoCardId "crd_11dcd18187623c73"
          , acdxProgram      = "paytogether"
          , acdxDesign       = UnknownDesign "blue"
          , acdxLastFour     = CardLastFour "6046"
          , acdxStatus       = CardActive
          , acdxActivatedAt  = Just $ stringToTime "2019-09-04T04:45:24+00:00"
          , acdxCreatedAt    = stringToTime "2019-09-04T04:45:24+00:00"
          , acdxCardholderId = AptoCardholderId "crdhldr_a"
          , acdxDDANumber    = Nothing
          , acdxABARouting   = Nothing
          }
        msg = AptoCardResponse [card1, card2]
      someJson <- readFile "test/json/messages/apto-createcard.json"
      A.eitherDecode someJson `shouldBe` Right msg
