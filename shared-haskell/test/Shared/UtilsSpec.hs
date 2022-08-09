{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module Shared.UtilsSpec
  ( spec
  )
where

import           Test.Hspec
import           Shared.Utils

spec :: Spec
spec = parallel $ describe "Base64" $ do
  describe "Base64 -> Base64URL" $ do
    it "has padding" $ do
      b64toB64URL "hJQWHABDBjoPHorYF5xghQ==" `shouldBe` "hJQWHABDBjoPHorYF5xghQ"
    it "has no padding" $ do
      b64toB64URL "PEM6Q09ETT8/Pz8+TFxdW3BvaXV5dHJl"
        `shouldBe` "PEM6Q09ETT8_Pz8-TFxdW3BvaXV5dHJl"
    it "https://base64.guru/standards/base64url" $ do
      b64toB64URL "PDw/Pz8+Pg==" `shouldBe` "PDw_Pz8-Pg"

  describe "Base64URL -> Base64" $ do
    it "has padding" $ do
      b64URLToB64 "hJQWHABDBjoPHorYF5xghQ" `shouldBe` "hJQWHABDBjoPHorYF5xghQ=="
    it "has no padding" $ do
      b64URLToB64 "PEM6Q09ETT8_Pz8-TFxdW3BvaXV5dHJl"
        `shouldBe` "PEM6Q09ETT8/Pz8+TFxdW3BvaXV5dHJl"
    it "https://base64.guru/standards/base64url" $ do
      b64URLToB64 "PDw_Pz8-Pg" `shouldBe` "PDw/Pz8+Pg=="
