{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module App.GetBalance.PureSpec
  ( spec
  ) where

import           Control.Exception              ( evaluate )
import           PaymentAuth.App.GetBalance     ( foldCurrency )
import           Shared.Models.Currency         ( Currency(..) )
import           Test.Hspec                     ( Spec
                                                , anyException
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldThrow
                                                )

spec :: Spec
spec = parallel $ do
  -- let paymentsAre    = const . return
  -- let transactionAre = const . return

  describe "foldCurrency" $ do
    it "USD" $ do
      foldCurrency [Currency "USD" 0, Currency "USD" 1.2, Currency "USD" (-1.3)]
        `shouldBe` Currency "USD" (-0.1)

    it "EUR" $ do
      foldCurrency [Currency "EUR" 0, Currency "EUR" 1.2, Currency "EUR" (-1.3)]
        `shouldBe` Currency "EUR" (-0.1)

    it "empty" $ do
      foldCurrency [] `shouldBe` Currency "USD" 0.0

    it "mixed throws" $ do
      evaluate (foldCurrency [Currency "EUR" 1.1, Currency "USD" 1.2])
        `shouldThrow` anyException
