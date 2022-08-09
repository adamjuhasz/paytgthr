{- HLINT ignore "Redundant do" -}
module App.RiskManagementSpec where

import           Control.Exception              ( evaluate )
import           Data.Ratio                     ( (%) )
import           PaymentAuth.App.RiskManagement ( approveTransaction
                                                , maxTransactionAmount
                                                )
import           Scaffolding.Transactions       ( asos )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                , userJulian
                                                )
import           Shared.Models.Currency         ( Currency(Currency)
                                                , roundUpUSD
                                                )
import           Shared.Models.RiskScore
import           Shared.Models.Transaction      ( DeclineReason
                                                  ( ExceedMaxTrxAmount
                                                  , LowBalance
                                                  , P2PNotAllowed
                                                  , RiskyMerchant
                                                  )
                                                , MastercardMCC(MastercardMCC)
                                                , MerchantInfo
                                                  ( CardMerchant
                                                  , cmiCountry
                                                  , cmiLocality
                                                  , cmiMcc
                                                  , cmiMccDesc
                                                  , cmiName
                                                  , cmiRegion
                                                  )
                                                , Transaction
                                                  ( trxDescription
                                                  , trxDisplayAmount
                                                  , trxMerchant
                                                  , trxSplitAmounts
                                                  , trxState
                                                  )
                                                , TransactionState
                                                  ( TrxAuthorized
                                                  , TrxDeclined
                                                  )
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult
                                                  ( InsufficentFunds
                                                  , RiskTriggered
                                                  , Success
                                                  )
                                                , RiskRules
                                                  ( ExceedMaxPerTrxAmount
                                                  , RiskyMerchantName
                                                  , RiskyP2P
                                                  )
                                                )
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
  describe "approveTransaction" $ do
    describe "catches errors" $ do
      it "empty balance"
        $             evaluate
                        (approveTransaction (exampleRiskScore { rskTrustScore = 100 })
                                            asos
                                            []
                        )
        `shouldThrow` anyException

      it "empty split"
        $             evaluate
                        (approveTransaction
                          (exampleRiskScore { rskTrustScore = 100 })
                          (asos { trxSplitAmounts = [] })
                          [ (usrUserID userJohn, Currency "USD" 100)
                          , (usrUserID userJane, Currency "USD" 100)
                          ]
                        )
        `shouldThrow` anyException

      it "missing balance"
        $             evaluate
                        (approveTransaction (exampleRiskScore { rskTrustScore = 100 })
                                            asos
                                            [(usrUserID userJane, Currency "USD" 100)]
                        )
        `shouldThrow` anyException

      it "wrong balance"
        $             evaluate
                        (approveTransaction
                          (exampleRiskScore { rskTrustScore = 100 })
                          asos
                          [ (usrUserID userJohn  , Currency "USD" 100)
                          , (usrUserID userJulian, Currency "USD" 100)
                          ]
                        )
        `shouldThrow` anyException

      it "missing split"
        $             evaluate
                        (approveTransaction
                          (exampleRiskScore { rskTrustScore = 100 })
                          (asos { trxSplitAmounts = [(usrUserID userJohn, 50)] })
                          [ (usrUserID userJohn, Currency "USD" 100)
                          , (usrUserID userJane, Currency "USD" 100)
                          ]
                        )
        `shouldThrow` anyException

    describe "50:50 split" $ do
      it "reject is below account requirements" $ do
        let expected =
              asos { trxState = TrxDeclined (LowBalance [usrUserID userJohn]) }
        let bankBalance = Currency "USD" 10
        let res = approveTransaction
              (exampleRiskScore { rskTrustScore = 100 })
              asos
              [ (usrUserID userJohn, bankBalance)
              , (usrUserID userJane, bankBalance)
              ]

        res
          `shouldBe` ( expected
                     , InsufficentFunds
                       expected
                       [ ( usrUserID userJohn
                         , roundUpUSD (Currency "USD" (524 % 25) * 0.6)
                         , bankBalance
                         )
                       ] --12.58
                     )

      it "accepts" $ do
        let expected    = asos { trxState = TrxAuthorized }
            trxAmt      = Currency "USD" 500
            bankBalance = Currency "USD" 500
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            asos { trxDisplayAmount = trxAmt }
            [ (usrUserID userJohn, bankBalance)
            , (usrUserID userJane, bankBalance)
            ]
          `shouldBe` ( expected { trxDisplayAmount = trxAmt }
                     , Success $ expected { trxDisplayAmount = trxAmt }
                     )

      it "rejects if trx is larger than max val" $ do
        let maxAmt  = maxTransactionAmount
        let trxAmt  = maxTransactionAmount + Currency "USD" 1
        let bankAmt = Currency "USD" 50000
        let expected = asos
              { trxState         = TrxDeclined (ExceedMaxTrxAmount maxAmt)
              , trxDisplayAmount = trxAmt
              }
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            (asos { trxDisplayAmount = trxAmt })
            [(usrUserID userJohn, bankAmt), (usrUserID userJane, bankAmt)]
          `shouldBe` ( expected
                     , RiskTriggered expected (ExceedMaxPerTrxAmount maxAmt)
                     )

      it "reject if reserve not met (>1.0 reserve factor)" $ do
        let trxAmt     = Currency "USD" 100
            smlBankAmt = Currency "USD" 50
            lrgBankAmt = Currency "USD" 50000
            expected   = asos
              { trxState         = TrxDeclined (LowBalance [usrUserID userJohn])
              , trxDisplayAmount = trxAmt
              }
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            (asos { trxDisplayAmount = trxAmt })
            [(usrUserID userJohn, smlBankAmt), (usrUserID userJane, lrgBankAmt)]
          `shouldBe` ( expected
                     , InsufficentFunds
                       expected
                       [(usrUserID userJohn, trxAmt * 0.6, smlBankAmt)]
                     )

      it "accept if just at reserve" $ do
        let
          trxAmt     = Currency "USD" 100
          smlBankAmt = Currency "USD" 100
          lrgBankAmt = Currency "USD" 50000
          expected =
            asos { trxState = TrxAuthorized, trxDisplayAmount = trxAmt }
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            (asos { trxDisplayAmount = trxAmt })
            [(usrUserID userJohn, smlBankAmt), (usrUserID userJane, lrgBankAmt)]
          `shouldBe` (expected, Success expected)

    describe "100:0 split" $ do
      let splits = [(usrUserID userJohn, 100 % 1), (usrUserID userJane, 0 % 1)]
      it "reject is below account requirements" $ do
        let expected = asos { trxState         = TrxAuthorized
                            , trxSplitAmounts  = splits
                            , trxDisplayAmount = Currency "USD" 100
                            }
            smlBankBalance = Currency "USD" 100
            lrgBankBalance = Currency "USD" 1000
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            expected
            [ (usrUserID userJohn, smlBankBalance)
            , (usrUserID userJane, lrgBankBalance)
            ]
          `shouldBe` (expected, Success expected)

    describe "Risk Mangement" $ do
      let splits = [(usrUserID userJohn, 50 % 1), (usrUserID userJane, 50 % 1)]
      it "rejects cash app" $ do
        let expected = asos
              { trxState         = TrxDeclined P2PNotAllowed
              , trxSplitAmounts  = splits
              , trxDisplayAmount = Currency "USD" 1
              , trxDescription   = Just ""
              , trxMerchant      = Just
                                     (CardMerchant { cmiMcc = MastercardMCC "4829"
                                                   , cmiMccDesc  = ""
                                                   , cmiName     = ""
                                                   , cmiLocality = Just ""
                                                   , cmiRegion   = Nothing
                                                   , cmiCountry  = ""
                                                   }
                                     )
              }
            smlBankBalance = Currency "USD" 100
            lrgBankBalance = Currency "USD" 1000
        approveTransaction
            (exampleRiskScore { rskTrustScore = 10 })
            expected
            [ (usrUserID userJohn, smlBankBalance)
            , (usrUserID userJane, lrgBankBalance)
            ]
          `shouldBe` (expected, RiskTriggered expected RiskyP2P)

      it "accpets MCC with Medium Risk" $ do
        let expected = asos
              { trxState         = TrxAuthorized
              , trxSplitAmounts  = splits
              , trxDisplayAmount = Currency "USD" 1
              , trxDescription   = Just ""
              , trxMerchant      = Just $ CardMerchant
                                     { cmiMcc      = MastercardMCC "5734"
                                     , cmiMccDesc  = ""
                                     , cmiName     = ""
                                     , cmiLocality = Just ""
                                     , cmiRegion   = Just ""
                                     , cmiCountry  = ""
                                     }
              }
            smlBankBalance = Currency "USD" 100
            lrgBankBalance = Currency "USD" 1000
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            expected
            [ (usrUserID userJohn, smlBankBalance)
            , (usrUserID userJane, lrgBankBalance)
            ]
          `shouldBe` (expected, Success expected)

      it "rejects giftcards through merchant name" $ do
        let expected = asos
              { trxState         = TrxDeclined RiskyMerchant
              , trxSplitAmounts  = splits
              , trxDisplayAmount = Currency "USD" 1
              , trxDescription   = Nothing
              , trxMerchant      = Just
                                     (CardMerchant { cmiMcc = MastercardMCC "7399"
                                                   , cmiMccDesc = ""
                                                   , cmiName = "BHN*GIFTCARDS"
                                                   , cmiLocality = Just ""
                                                   , cmiRegion = Nothing
                                                   , cmiCountry = ""
                                                   }
                                     )
              }
            smlBankBalance = Currency "USD" 100
            lrgBankBalance = Currency "USD" 1000
        approveTransaction
            (exampleRiskScore { rskTrustScore = 10 })
            expected
            [ (usrUserID userJohn, smlBankBalance)
            , (usrUserID userJane, lrgBankBalance)
            ]
          `shouldBe` (expected, RiskTriggered expected RiskyMerchantName)

      it "rejects giftcards through description" $ do
        let expected = asos
              { trxState         = TrxDeclined RiskyMerchant
              , trxSplitAmounts  = splits
              , trxDisplayAmount = Currency "USD" 1
              , trxDescription   = Just "BHN*GIFTCARDS"
              , trxMerchant      = Just
                                     (CardMerchant { cmiMcc = MastercardMCC "7399"
                                                   , cmiMccDesc  = ""
                                                   , cmiName     = ""
                                                   , cmiLocality = Just ""
                                                   , cmiRegion   = Nothing
                                                   , cmiCountry  = ""
                                                   }
                                     )
              }
            smlBankBalance = Currency "USD" 100
            lrgBankBalance = Currency "USD" 1000
        approveTransaction
            (exampleRiskScore { rskTrustScore = 10 })
            expected
            [ (usrUserID userJohn, smlBankBalance)
            , (usrUserID userJane, lrgBankBalance)
            ]
          `shouldBe` (expected, RiskTriggered expected RiskyMerchantName)

      it "accepts affirm payments" $ do
        let expected = asos
              { trxState         = TrxAuthorized
              , trxSplitAmounts  = splits
              , trxDisplayAmount = Currency "USD" 1
              , trxDescription   = Just "AFFIRM *PAYMENT"
              , trxMerchant      = Just
                                     (CardMerchant { cmiMcc = MastercardMCC "6012"
                                                   , cmiMccDesc = ""
                                                   , cmiName = "AFFIRM *PAYMENT"
                                                   , cmiLocality = Just ""
                                                   , cmiRegion = Nothing
                                                   , cmiCountry = ""
                                                   }
                                     )
              }
            smlBankBalance = Currency "USD" 100
            lrgBankBalance = Currency "USD" 1000
        approveTransaction
            (exampleRiskScore { rskTrustScore = 100 })
            expected
            [ (usrUserID userJohn, smlBankBalance)
            , (usrUserID userJane, lrgBankBalance)
            ]
          `shouldBe` (expected, Success expected)

