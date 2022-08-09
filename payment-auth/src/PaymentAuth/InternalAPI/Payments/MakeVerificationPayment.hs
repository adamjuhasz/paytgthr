{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.InternalAPI.Payments.MakeVerificationPayment where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Payments.MakeAPayment
                                                ( VerificationPaymentSource(..)
                                                , makeInitialVerificationPayment
                                                )
import           PaymentAuth.InternalAPI.Payments.ProcessEffects
                                                ( processPaymentEffects )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant                        ( NoContent(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.PaymentAuth.API  ( MakeVerificationPaymentBody(..)
                                                , TraceContext
                                                , VerificationPaymentStyle(..)
                                                )

initiateVerificationPayments
  :: ( HasAccounts m
     , HasEventTracking m
     , HasRandom m
     , HasPaymentsDB m
     , HasDwollaClient m
     , HasLedgerDB m
     , MonadIO m
     , HasTime m
     )
  => TraceContext
  -> UserID
  -> MakeVerificationPaymentBody
  -> m NoContent
initiateVerificationPayments trace uid MakeVerificationPaymentBody {..} = do
  let normalizeAmt = case verificationStyle of
        UserSavedAmounts -> UseUsersAmounts
        UseSpecic ds     -> UseTheseAmounts ds

  tracePrint trace "initiateVerificationPayments " (uid, verificationStyle)

  events <- makeInitialVerificationPayment trace uid normalizeAmt
  mapM_ (processPaymentEffects trace) events

  return NoContent
