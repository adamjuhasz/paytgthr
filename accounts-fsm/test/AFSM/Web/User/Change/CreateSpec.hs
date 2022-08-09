{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

module AFSM.Web.User.Change.CreateSpec where

import           AFSM.Web.User.Change.Create    ( isBannedDomain )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

spec :: Spec
spec = parallel $ do
  describe "isBannedDomain" $ do
    it "bans yop" $ do
      isBannedDomain "jacksmidtorla@example.com" `shouldBe` True
      isBannedDomain " jacksmidtorla@example.com" `shouldBe` True
      isBannedDomain "yopmail.com@example.com" `shouldBe` True

    it "happy path" $ do
      isBannedDomain " hagagaibaayggshshsi8@example.com" `shouldBe` False
      isBannedDomain " adam@example.com" `shouldBe` False
      isBannedDomain " adam@akitram.co" `shouldBe` False
      isBannedDomain " yopmail@akitram.co" `shouldBe` False
      isBannedDomain "carolina_dasfaf343@example.com" `shouldBe` False
      isBannedDomain "dsf3fsdfs@example.com" `shouldBe` False
