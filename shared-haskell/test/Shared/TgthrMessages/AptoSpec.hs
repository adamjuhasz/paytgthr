{-# LANGUAGE OverloadedStrings #-}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}


module Shared.TgthrMessages.AptoSpec
  ( spec
  )
where

import           Prelude                 hiding ( readFile
                                                , filter
                                                )
import           Test.Hspec
import qualified Data.UUID                     as U
import qualified Data.Aeson                    as A
import           Data.ByteString.Lazy           ( readFile
                                                , filter
                                                )
import           Data.Aeson.Encode.Pretty


import           Shared.TgthrMessages.Apto
import           Shared.Messages
import           Shared.Models.User
import           Shared.Models.Apto.Base
import           Utils

spec :: Spec
spec = parallel $ do
  describe "AptoCardholderCreated" $ do
    let c = AptoCardholderCreated
          { cheUser       = UserID U.nil
          , cheCardHolder = AptoCardholderId "crdhldr_a"
          }

    it "encodes -> decodes" $ do
      (A.fromJSON . A.toJSON) c `shouldBe` A.Success c
      (A.eitherDecode . A.encode) c `shouldBe` Right c


    it "encodes into kown TgthrMessage" $ do
      let msg = fullMsg { tgthrBody = EventV1 $ AptoEvt c }
      someJson <- filter (\x -> x /= 32 && x /= 10 && x /= 13)
        <$> readFile "test/json/messages/cardholdercreated.json"
      encodePretty' prettyConfig msg `shouldBe` someJson

    it "decodes from known TgthrMessage" $ do
      let msg = fullMsg { tgthrBody = EventV1 $ AptoEvt c }
      someJson <- readFile "test/json/messages/cardholdercreated.json"
      A.eitherDecode someJson `shouldBe` Right msg
