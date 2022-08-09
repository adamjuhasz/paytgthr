{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.GetBalance.OneDebitPaymentSpec
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
import           Scaffolding.Payments           ( paymentDebit )
import           Scaffolding.Users              ( userJohn )
import           Shared.Models.Currency         ( Currency(..) )
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
  getPendingTransactionsFor _ _ = return []
instance HasPaymentsDB TestMonad where
  getPendingPaymentsOf _ _ _ = return [paymentDebit]
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
  it "1 payment (debit only)" $ do
    let limitFor100Score = limitByLevel $ getUserLevelFromScore 100
    trace     <- randomTrace

    liability <- unTestMonad $ getLiability trace (usrUserID userJohn)
    liability `shouldBe` Currency "USD" 0

    unTestMonad (getSpendingLimit trace (usrUserID userJohn))
      `shouldReturn` limitFor100Score
