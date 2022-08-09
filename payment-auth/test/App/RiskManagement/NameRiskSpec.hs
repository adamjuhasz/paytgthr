{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use lambda-case" -}

module App.RiskManagement.NameRiskSpec
  ( spec
  )
where

import           PaymentAuth.App.RiskManagement.NameRisk
                                                ( nameRiskScore )
import           PaymentAuth.App.RiskManagement.Types
                                                ( MerchantRiskScore(..) )
import           Test.Hspec                     ( describe
                                                , it
                                                , shouldBe
                                                , Spec
                                                )

spec :: Spec
spec = do
  describe "Test normal should not be blocked" $ do
    it "list of known good" $ do
      nameRiskScore "SIMPLEMOBILE*AIRTIME" `shouldBe` None
      nameRiskScore "GFW*3GLDSCR.COM" `shouldBe` None
      nameRiskScore "TRYSUPERSNACKS" `shouldBe` None
      nameRiskScore "DUNKIN #306293 Q35" `shouldBe` None
      nameRiskScore "DOORDASH*WENDYS" `shouldBe` None
      nameRiskScore "Amazon Music 888-802-3080 WAUSA" `shouldBe` None
      nameRiskScore "GOOGLE*GOOGLE STORAGE G.CO HELPPAY#CAUSA" `shouldBe` None
      nameRiskScore "APPLE.COM/BILL www.apple.comCAUSA" `shouldBe` None

  describe "Test blacklisted" $ do
    it "list of known bad" $ do
      nameRiskScore "CASH APP*CRISTY A BABC" `shouldBe` VeryHigh
      nameRiskScore "WISH.COM" `shouldBe` VeryHigh
      nameRiskScore "VENMO NEW YORK NYUSA" `shouldBe` VeryHigh
      nameRiskScore "XOOM.COM" `shouldBe` VeryHigh
      nameRiskScore "MONEYGRAM US DALLAS TXUSA" `shouldBe` VeryHigh
      nameRiskScore "PAYPAL *FACEBOOKPAY" `shouldBe` VeryHigh
      nameRiskScore "JASSBY INC" `shouldBe` VeryHigh
      nameRiskScore "BHN*GIFTCARDS" `shouldBe` VeryHigh

    it "doesn't depend on case" $ do
      nameRiskScore "cash app*cristy a babc" `shouldBe` VeryHigh

  describe "Test whitelist" $ do
    it "list of known good" $ do
      nameRiskScore "AFFIRM *PAYMENT 855-423-3729 CAUSA"
        `shouldBe` WhitelistAllow

