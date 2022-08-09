{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module APIPrivacy.InternalAPI.CardHostedUISpec where

import           APIPrivacy.InternalAPI.CardHostedUI
                                                ( parsePCIHtml )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )


spec :: Spec
spec = parallel $ do
  describe "parsePCIHtml" $ do
    it "parses v1" $ do
      html   <- readFile "./test/html/Pciv1.html"
      parsed <- parsePCIHtml html
      parsed `shouldBe` Just ("5135135135135135", "05", "27", "987")

    it "rejects bad v1" $ do
      html   <- readFile "./test/html/Pciv1-error.html"
      parsed <- parsePCIHtml html
      parsed `shouldBe` Nothing

    it "rejects partial v1" $ do
      html   <- readFile "./test/html/Pciv1-partial.html"
      parsed <- parsePCIHtml html
      parsed `shouldBe` Nothing

    it "rejects empty v1" $ do
      html   <- readFile "./test/html/Pciv1-empty.html"
      parsed <- parsePCIHtml html
      parsed `shouldBe` Nothing

    it "parses v2" $ do
      html   <- readFile "./test/html/Pciv2.html"
      parsed <- parsePCIHtml html
      parsed `shouldBe` Just ("5135135135135135", "05", "27", "987")
