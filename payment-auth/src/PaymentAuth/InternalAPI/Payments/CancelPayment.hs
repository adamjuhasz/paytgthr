{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.Payments.CancelPayment where

import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Payments.CancelPayment
                                                ( cancelAPayment )
import           PaymentAuth.InternalAPI.Payments.ProcessEffects
                                                ( processPaymentEffects )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant                        ( NoContent(..) )
import           Shared.Models.Ids              ( PaymentId )
import           Shared.WebAPI.General.API      ( TraceContext )

cancelPayment
  :: ( HasPaymentsDB m
     , HasDwollaClient m
     , HasLedgerDB m
     , HasRandom m
     , HasTime m
     , HasAccounts m
     , HasRiskScoresDB m
     , MonadIO m
     , HasEventTracking m
     , MonadMask m
     )
  => TraceContext
  -> PaymentId
  -> m NoContent
cancelPayment trace pid = do
  evts <- cancelAPayment trace pid
  mapM_ (processPaymentEffects trace) evts

  return NoContent
