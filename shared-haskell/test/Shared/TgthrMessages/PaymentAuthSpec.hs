{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant do" -}

module Shared.TgthrMessages.PaymentAuthSpec
  ( spec
  ) where

import qualified Data.Aeson                    as A
import           Data.Maybe                     ( fromJust )
import qualified Data.UUID                     as U
import           Shared.Models.Currency         ( Currency(Currency) )
import           Shared.Models.User             ( UserID(UserID) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( PaymentCmd
                                                  ( AddToken
                                                  , GetSpendableBalance
                                                  )
                                                , PaymentReplies
                                                  ( AccountListReply
                                                  , GetBalanceReply
                                                  )
                                                , PlaidAccountId(PlaidAccountId)
                                                , PlaidEnvironment(Sandbox)
                                                , PublicToken(PublicToken)
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

spec :: Spec
spec = parallel $ do
  let gBC = GetSpendableBalance (UserID U.nil)
      gBR = GetBalanceReply (UserID U.nil) (Currency "USD" 100.12)
      aT  = AddToken (UserID U.nil) (PublicToken "abc") Sandbox

  describe "JSON" $ do
    it "BalanceRefreshRequest"
      $          (fromJust . A.decode . A.encode) gBC
      `shouldBe` gBC
    it "GetBalanceReply" $ (fromJust . A.decode . A.encode) gBR `shouldBe` gBR
    it "AddToken" $ (fromJust . A.decode . A.encode) aT `shouldBe` aT

  describe "Known Formats"
    $          it "BalanceRefreshRequest"
    $          A.encode gBC
    `shouldBe` (  ""
               <> "{"
               <> "\"kind\":\"getspendablebalance\""
               <> ","
               <> "\"user\":\"00000000-0000-0000-0000-000000000000\""
               <> "}"
               )

  describe "GetBalanceReply" $ do
    it "encodes -> decodes" $ do
      let orig = GetBalanceReply (UserID U.nil) (Currency "USD" 0.0)
      (A.eitherDecode . A.encode $ orig) `shouldBe` Right orig

  describe "AccountListReply" $ it "encodes -> decodes" $ do
    let orig = AccountListReply
          (UserID U.nil)
          [(PlaidAccountId "123", "234", "345", "abc", Currency "USD" 100)]
    (A.eitherDecode . A.encode $ orig) `shouldBe` Right orig

