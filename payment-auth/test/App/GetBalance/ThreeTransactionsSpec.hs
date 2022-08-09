{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.GetBalance.ThreeTransactionsSpec
  ( spec
  ) where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Decimal                   ( DecimalRaw
                                                , roundTo
                                                )
import           Data.Maybe                     ( fromJust )
import qualified Data.Time.Clock               as Clock
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           PaymentAuth.App.GetBalance     ( getSpendingLimit )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Transactions       ( asos
                                                , asosAmount
                                                )
import           Scaffolding.Users              ( userJohn )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.RiskScore        ( RiskFact(InitialRisk)
                                                , RiskScore(..)
                                                , getUserLevelFromScore
                                                , limitByLevel
                                                )
import           Shared.Models.Transaction      ( Transaction(trxSplitAmounts) )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.WebAPI.General.API
import           Test.Hspec                     ( Spec
                                                , it
                                                , parallel
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
  getPendingTransactionsFor _ _ = return $ replicate 3 asos
instance HasPaymentsDB TestMonad where
  getPendingPaymentsOf _ _ _ = return []
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

toMoney :: Currency -> Currency
toMoney (Currency iso val) = Currency
  iso
  ( (toRational :: DecimalRaw Integer -> Rational)
  . roundTo 2
  . fromRational
  $ val
  )

spec :: Spec
spec = parallel $ do
  it "3 transactions" $ do
    let usersSplit =
          (/ 100)
            . fromRational
            . fromJust
            . lookup (usrUserID userJohn)
            $ trxSplitAmounts asos
    let limitFor100Score = limitByLevel $ getUserLevelFromScore 100

    trace <- randomTrace
    unTestMonad (getSpendingLimit trace (usrUserID userJohn))
      `shouldReturn` (limitFor100Score - (toMoney (asosAmount * usersSplit) * 3)
                     )
