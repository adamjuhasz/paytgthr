{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use lambda-case" -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module Callback.AuthorizeTransaction.NoGroupsSpec
  ( spec
  ) where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Time.Clock               as Clock
import           Data.UUID                      ( nil )
import           GHC.IO.Unsafe                  ( unsafePerformIO )
import           PaymentAuth.App.Purchases.AuthorizeTransaction
                                                ( authorizeNewTransaction )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Scaffolding.Transactions       ( basicTrx )
import           Scaffolding.Users              ( userJane
                                                , userJohn
                                                )
import           Shared.Models.Card             ( IssuerPlatform(PayWithPrivacy)
                                                , PrivacyCardToken(..)
                                                )
import           Shared.Models.RiskScore        ( RiskFact(..)
                                                , RiskScore(..)
                                                )
import           Shared.Models.Transaction      ( Transaction(..) )
import           Shared.Models.User             ( UserID(..)
                                                , UserModel(..)
                                                )
import           Shared.TgthrMessages.Base      ( MessageID(..) )
import           Shared.TgthrMessages.PaymentAuth
                                                ( AuthResult(..)
                                                , extractTrxFromAuthResult
                                                )
import           Shared.WebAPI.General.API
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , parallel
                                                , shouldBe
                                                , shouldSatisfy
                                                )

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

instance HasAccounts TestMonad where
  getGroupFor _ _ = return Nothing
  getUser _ = findUsers
  findCard _ _ = return Nothing
instance HasTime TestMonad where
  getCurrentTime = liftIO Clock.getCurrentTime
instance HasLedgerDB TestMonad where
  getLedgerJournalType _ _ = return []
instance HasTransactionsDB TestMonad where
  getTransactionUsingSourceId _ _ = return Nothing
  getPendingTransactionsFor _ _ = return []
  saveTransaction _ _ = return ()
instance HasRandom TestMonad where
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
instance HasEventTracking TestMonad where

spec :: Spec
spec = parallel $ do
  describe "Solo purchases not allowed" $ do
    it "Can't get a group for John, trx declined" $ do
      trace    <- midToTrace (MessageID nil)
      (res, _) <- unTestMonad $ authorizeNewTransaction
        trace
        (PayWithPrivacy $ PrivacyCardToken "")
        basicTrx

      -- verify we approved Trx
      res
        `shouldSatisfy` (\a -> case a of
                          GroupNotActive _ -> True
                          _                -> False
                        )
      -- grab transaction for authorization data structure
      let finalTrx = extractTrxFromAuthResult res
      -- there should be no group b/c it's a solo purchase
      trxGroupId finalTrx `shouldBe` Nothing
      -- only john should be splitting, and at 100%
      trxSplitAmounts finalTrx `shouldBe` [(usrUserID userJohn, 100)]

findUsers :: (Monad m) => UserID -> m (Maybe UserModel)
findUsers u | u == usrUserID userJohn = return $ Just userJohn
            | u == usrUserID userJane = return $ Just userJane
            | otherwise               = error "forgot to add this user"

