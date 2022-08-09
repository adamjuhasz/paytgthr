
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI where

import           Control.Concurrent             ( tryReadMVar )
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO )
import           Control.Monad.Reader           ( MonadIO(..)
                                                , MonadReader
                                                , asks
                                                )
import           Control.Monad.Trans.Reader     ( runReaderT )
import           Data.Text                      ( Text )
import           Network.Wai                    ( Application )
import           PaymentAuth.AppMonad           ( AppSettings(..)
                                                , AppWebM(..)
                                                )
import           PaymentAuth.InternalAPI.Admin.InitiatePaymentsFromLedger
                                                ( initiatePaymentsFromLedger )
import           PaymentAuth.InternalAPI.Admin.RefreshPlaidBalances
                                                ( refreshPlaidBalances )
import           PaymentAuth.InternalAPI.Ledger.CreateJournal
                                                ( createJournal )
import           PaymentAuth.InternalAPI.Ledger.FindJournal
                                                ( findThisJournal )
import           PaymentAuth.InternalAPI.Ledger.GetEntries
                                                ( getEntries )
import           PaymentAuth.InternalAPI.Payments.CancelPayment
                                                ( cancelPayment )
import           PaymentAuth.InternalAPI.Payments.GetPayment
                                                ( getFromSourceId
                                                , getPaymentFromDB
                                                , getPayments
                                                )
import           PaymentAuth.InternalAPI.Payments.MakeVerificationPayment
                                                ( initiateVerificationPayments )
import           PaymentAuth.InternalAPI.Payments.PayOutLedger
                                                ( payLedger )
import           PaymentAuth.InternalAPI.Payments.ToBeScheduled
                                                ( usersNeedingScheduledPayments
                                                )
import           PaymentAuth.InternalAPI.Payments.UpdatePayment
                                                ( updateAPayment )
import           PaymentAuth.InternalAPI.Purchase.Authorize
                                                ( autorizeTransaction )
import           PaymentAuth.InternalAPI.Purchase.Query
                                                ( getATransaction
                                                , queryBySourceId
                                                )
import           PaymentAuth.InternalAPI.Purchase.Set
                                                ( setPurcahseState )
import           PaymentAuth.InternalAPI.Purchase.Update
                                                ( updatePurchase )
import           PaymentAuth.InternalAPI.User.GetLiability
                                                ( getLiability )
import           PaymentAuth.InternalAPI.User.GetSpendableBalance
                                                ( getSpendbaleBalance )
import           PaymentAuth.InternalAPI.User.Ledger
                                                ( adjustUsersLedger )
import           PaymentAuth.InternalAPI.User.ManualBankFS
                                                ( setManualBankFS
                                                , verifyManualFS
                                                )
import           PaymentAuth.InternalAPI.User.Plaid
                                                ( setAccountWithPlaid )
import           PaymentAuth.InternalAPI.User.RiskScore
                                                ( getRiskScore )
import           PaymentAuth.InternalAPI.User.Transactions
                                                ( getTransactions )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.HttpClient   ( HasHttpClient )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB )
import           PaymentAuth.Plaid.LinkTokens   ( getLinkToken )
import           Servant                        ( Handler
                                                , ServerError
                                                , err503
                                                , throwError
                                                )
import           Servant.Server.Generic         ( AsServerT
                                                , genericServeT
                                                )
import           Shared.WebAPI.PaymentAuth.API  ( Routes(..) )

genServerHandler
  :: ( HasLedgerDB m
     , HasTransactionsDB m
     , HasPaymentsDB m
     , HasRiskScoresDB m
     , HasPlaidDB m
     , HasTime m
     , HasAccounts m
     , HasRandom m
     , HasEventTracking m
     , HasDwollaClient m
     , HasHttpClient m
     , MonadIO m
     , MonadError ServerError m
     , MonadMask m
     , MonadReader AppSettings m
     )
  => Routes (AsServerT m)
genServerHandler = Routes
  { _GetSpendableBalance       = getSpendbaleBalance
  , _MakeVerificationPayment   = initiateVerificationPayments
  , _GetPlaidLinkToken         = getLinkToken
  , _SetFSBankManual           = setManualBankFS
  , _VerifyBankAccount         = verifyManualFS
  , _SetPlaidAccount           = setAccountWithPlaid
  , _AuthorizeCardPurchase     = autorizeTransaction
  , _GetTrustScore             = getRiskScore
  , _GetPurchases              = getTransactions
  , _GetLiability              = getLiability
  , _GetPayments               = getPayments
  , _PayoutLedger              = payLedger

  -- Payments
  , _GetPayment                = getPaymentFromDB
  , _UpdatePayment             = updateAPayment
  , _QueryPaymentSourceId      = getFromSourceId
  , _CancelPayment             = cancelPayment

  -- Purchase
  , _GetPurchase               = getATransaction
  , _UpdatePurchase            = updatePurchase
  , _QueryPurchaseSourceId     = queryBySourceId
  , _GetUsersNeedToRepayUs     = usersNeedingScheduledPayments
  , _SetPurchaseStateDirectly  = setPurcahseState

  -- Ledger
  , _CreateJournal             = createJournal
  , _CreateJournalTrx          = adjustUsersLedger
  , _GetJournal                = findThisJournal
  , _GetEntries                = getEntries

  -- Admin
  , _AdminLedgerToPayments     = initiatePaymentsFromLedger
  , _AdminRefreshPlaidBalances = refreshPlaidBalances
  -- System
  , _health                    = healthHandler
  }

healthHandler
  :: (MonadReader AppSettings m, MonadError ServerError m, MonadIO m) => m Text
healthHandler = do
  mvar <- asks isShuttingDown
  res  <- liftIO $ tryReadMVar mvar
  case res of
    Nothing    -> return "PaymentAuth"
    Just False -> return "PaymentAuth"
    Just True  -> do
      liftIO $ putStrLn "SIGTERM received... returning 503 Internal"
      throwError err503

-- no need to change below

appToHandler :: AppSettings -> AppWebM m -> Handler m
appToHandler cfg app = runReaderT (unAppWebM app) cfg

internalApiApp :: AppSettings -> Application
internalApiApp cfg = genericServeT (appToHandler cfg) genServerHandler
