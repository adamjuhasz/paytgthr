{- HLINT ignore "Redundant do" -}

module Shared.WebAPI.General.APISpec
  ( spec
  ) where

import           Data.UUID.V4                   ( nextRandom )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.WebAPI.General.API      ( traceToMID
                                                , uuidToTrace
                                                , TraceContext(..)
                                                , incrementTrace
                                                )
import           Test.Hspec                     

spec :: Spec
spec = parallel $ do
  describe "Trace <-> MessageID" $ do
    it "goes back and forth" $ do
      randomUUID <- nextRandom
      let trace               = uuidToTrace randomUUID
      let (MessageID midUUID) = traceToMID trace
      randomUUID `shouldBe` midUUID
      
  describe "TraceContext" $ do
    it "increments it" $ do
      randomUUID <- nextRandom
      let trace  = uuidToTrace randomUUID
      newTrace <- incrementTrace trace
      spanId trace `shouldNotBe` spanId newTrace