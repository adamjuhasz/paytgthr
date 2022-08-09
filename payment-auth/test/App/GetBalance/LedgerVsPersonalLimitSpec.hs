{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.GetBalance.LedgerVsPersonalLimitSpec
  ( spec
  ) where

import           Control.Monad.IO.Class         ( MonadIO )
import           Data.Time                      ( UTCTime )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           PaymentAuth.App.GetBalance     ( getLiability
                                                , getSpendingLimit
                                                )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Users              ( userJohn )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Ids
import           Shared.Models.Ledger.Common    ( LedgerFact(Manual) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.Models.Ledger.Journal
import           Shared.Models.RiskScore        ( RiskFact(InitialRisk)
                                                , RiskScore(..)
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.Utils                   ( stringToTime )
import           Shared.WebAPI.General.API
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                )

creditBalance :: Currency
creditBalance = Currency "USD" 200

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      )

aTime :: UTCTime
aTime = stringToTime "2019-10-04T20:18:14.284+00:00"

entry :: LedgerEntry
entry = LedgerEntry { lenId             = LedgerEntryId nil
                    , lenUser           = Just $ usrUserID userJohn
                    , lenRevision       = 1
                    , lenVersion        = "1.0"
                    , lenMsgSource      = MessageID nil
                    , lenBalance        = creditBalance
                    , lenFact           = Manual creditBalance
                    , lenIdempotency    = Nothing
                    , lenCreatedAt      = aTime
                    , lenJournal        = JournalId nil
                    , lenPendingBalance = Currency "USD" 0
                    , lenTransaction    = Nothing
                    }

instance HasLedgerDB TestMonad where
  getLedgerJournalType _ (GetPayTgthr _) = return
    [ LedgerJournal { journalId             = JournalId nil
                    , journalType           = VirtualAccount
                    , journalName           = ""
                    , journalUser           = Nothing
                    , lastJournalEntry      = lenId entry
                    , journalBalance        = lenBalance entry
                    , journalPendingBalance = lenPendingBalance entry
                    , journalRevision       = 1
                    , journalCreated        = aTime
                    , journalUpdated        = aTime
                    , journalTransaction    = LedgerTrxId nil
                    }
    ]
  getLedgerJournalType _ _ = return []
instance HasTransactionsDB TestMonad where
  getPendingTransactionsFor _ _ = return []
instance HasPaymentsDB TestMonad where
  getPendingPaymentsOf _ _ _ = return []
instance HasRiskScoresDB TestMonad where
  getRiskScoreOf trace u = return $ RiskScore
    { rskUser       = u
    , rskRev        = 1
    , rskTrustScore = 10
    , rskChange     = 0
    , rskFact       = InitialRisk
    , rskMsgSource  = traceToMID trace
    , rskCreatedAt  = unsafePerformIO Clock.getCurrentTime
    }

spec :: Spec
spec = parallel $ do
  describe "Ledger value overrides personal limit" $ do
    it "User has $200 credit and $20 personal limit, can spend $220" $ do
      trace    <- randomTrace

      liabilty <- unTestMonad $ getLiability trace (usrUserID userJohn)
      -- verify we have the credit above
      liabilty `shouldBe` creditBalance

      spendingLimit <- unTestMonad $ getSpendingLimit trace (usrUserID userJohn)

      -- verify we can spend the credit PLUS the personal limit
      -- Credit ($200) + Personal Limit ($20) = $220 max spendable
      spendingLimit `shouldBe` (creditBalance + Currency "USD" 75)
