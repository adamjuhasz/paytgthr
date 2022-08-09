{- HLINT ignore "Redundant do" -}

module Shared.TgthrMessages.AnalyticsSpec
  ( spec
  )
where

import           Test.Hspec
import qualified Data.Aeson                    as A
import           Data.Maybe

import           Shared.TgthrMessages.Analytics

spec :: Spec
spec = parallel $ do
  it "encodes -> decodes" $ do
    let wR = WebRequestedV1 "GET" "/" "Safari" "123" []
    (fromJust . A.decode . A.encode) wR `shouldBe` wR
