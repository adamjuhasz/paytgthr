{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.GetBalance.ThreePaymentOneTrxSpec
  ( spec
  ) where

import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Time.Clock               as Clock
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           PaymentAuth.App.GetBalance     ( getLiability
                                                , getSpendingLimit
                                                )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Payments           ( paymentAmount
                                                , paymentCredit
                                                , paymentDebit
                                                )
import           Scaffolding.Transactions       ( asos
                                                , asosAmount
                                                )
import           Scaffolding.Users              ( userJohn )
import           Shared.Models.Currency         ( roundDownUSD )
import           Shared.Models.RiskScore        ( RiskFact(InitialRisk)
                                                , RiskScore(..)
                                                , getUserLevelFromScore
                                                , limitByLevel
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.WebAPI.General.API
import           Test.Hspec                     ( Spec
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldReturn
                                                )

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )

instance HasLedgerDB TestMonad where
  getLedgerJournalType _ _ = return []
instance HasTransactionsDB TestMonad where
  getPendingTransactionsFor _ _ = return [asos]
instance HasPaymentsDB TestMonad where
  getPendingPaymentsOf _ _ _ =
    return [paymentDebit, paymentCredit, paymentCredit]
instance HasRiskScoresDB TestMonad where
  getRiskScoreOf trace u = return $ RiskScore
    { rskUser       = u
    , rskRev        = 1
    , rskTrustScore = 100
    , rskChange     = 0
    , rskFact       = InitialRisk
    , rskMsgSource  = traceToMID trace
    , rskCreatedAt  = unsafePerformIO Clock.getCurrentTime
    }

spec :: Spec
spec = parallel $ do
  it "1 transaction, 1 debit, 2 credit" $ do
    let limitFor100Score = limitByLevel $ getUserLevelFromScore 100
    let expectedLiability =
          ((-1) * 2 * paymentAmount) -- 2x credit payments
                                     + ((-1) * roundDownUSD (asosAmount * 0.6)) -- asos portion

    trace     <- randomTrace
    liability <- unTestMonad $ getLiability trace (usrUserID userJohn)
    -- expected $87.42
    -- paymentAmount => $50
    -- asosAmount => $20.97 (john pays 60% of this or $12.58)
    liability `shouldBe` expectedLiability

    unTestMonad (getSpendingLimit trace (usrUserID userJohn))
      `shouldReturn` (limitFor100Score + expectedLiability)
