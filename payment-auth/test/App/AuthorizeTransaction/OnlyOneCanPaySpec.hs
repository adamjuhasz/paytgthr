{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use lambda-case" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module App.AuthorizeTransaction.OnlyOneCanPaySpec
  ( spec
  ) where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           Data.UUID.V4                   ( nextRandom )
import           PaymentAuth.App.AuthorizeTransaction
                                                ( authorizeTransaction )
import           PaymentAuth.Monad.Accounts     ( HasAccounts
                                                  ( getRewardsForGroup
                                                  )
                                                )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.Apto.Transaction ( TransactionSource(Apto)
                                                , TransactionState
                                                  ( TrxAuthorized
                                                  )
                                                )
import           Shared.Models.Group            ( GroupId(..) )
import           Shared.Models.RiskScore        ( RiskFact(..)
                                                , RiskScore(..)
                                                , UserLevel(..)
                                                , limitByLevel
                                                )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionEvent(AuthRequest)
                                                , TransactionId(..)
                                                )
import           Shared.Models.User             ( UserModel(..) )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult(..) )
import           Shared.WebAPI.General.API
import           System.IO.Unsafe               ( unsafePerformIO )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldSatisfy
                                                )

aGroupId :: GroupId
{-# NOINLINE aGroupId #-}
aGroupId = unsafePerformIO (GroupId <$> nextRandom)

successTrx :: Transaction
successTrx = Transaction
  { trxId                = unsafePerformIO (TransactionId <$> nextRandom)
  , trxRevision          = 1
  , trxVersion           = "1.0"
  , trxMsgSource         = MessageID nil
  , trxState             = TrxAuthorized
  , trxSource            = Apto
  , trxSourceId          = "trx_1"
  , trxSourceEvent       = AuthRequest
  , trxUserId            = usrUserID userJohn
  , trxDisplayAmount     = limitByLevel Level1 * 3
  , trxBillingAmounts    = []
  , trxPurchasedAt       = unsafePerformIO Clock.getCurrentTime
  , trxDetails           = Nothing
  , trxGroupId           = Just (aGroupId, 1)
  , trxSourceIdempotency = Nothing
  , trxSplitAmounts      = [(usrUserID userJohn, 50), (usrUserID userJane, 50)]
  , trxMerchant          = Nothing
  , trxDescription       = Just "ABC INC"
  , trxAdjustments       = []
  , trxRewardId          = Nothing
  }

newtype TestMonad a =
    TestMonad { unTestMonad :: IO a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadThrow
      , MonadCatch
      , MonadMask
      )

instance HasLedgerDB TestMonad where
  getLedgerJournalType _ _ = return []
instance HasTransactionsDB TestMonad where
  getPendingTransactionsFor _ _ = return []
instance HasPaymentsDB TestMonad where
  getPendingPaymentsOf _ _ _ = return []
instance HasRiskScoresDB TestMonad where
  getRiskScoreOf trace u
    | u == usrUserID userJohn = return $ RiskScore
      { rskUser       = u
      , rskRev        = 1
      , rskTrustScore = 100
      , rskChange     = 0
      , rskFact       = InitialRisk
      , rskMsgSource  = traceToMID trace
      , rskCreatedAt  = unsafePerformIO Clock.getCurrentTime
      }
    | u == usrUserID userJane = return $ RiskScore
      { rskUser       = u
      , rskRev        = 1
      , rskTrustScore = 10
      , rskChange     = 0
      , rskFact       = InitialRisk
      , rskMsgSource  = traceToMID trace
      , rskCreatedAt  = unsafePerformIO Clock.getCurrentTime
      }
    | otherwise = error "user is bad"
instance HasAccounts TestMonad where
  getRewardsForGroup _ _ = return []

spec :: Spec
spec = parallel $ do
  describe "Only 1 of 2 people have enough" $ do
    it "1 person has enough" $ do
      trace <- midToTrace (MessageID nil)
      res   <- unTestMonad $ authorizeTransaction trace Nothing successTrx
      res
        `shouldSatisfy` (\a -> case a of
                          InsufficentFunds _ _ -> True
                          _                    -> False
                        )

