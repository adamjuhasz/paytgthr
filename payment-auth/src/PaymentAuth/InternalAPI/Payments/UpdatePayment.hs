{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.Payments.UpdatePayment where

import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Payments.UpdatePayment
                                                ( updatePayment )
import           PaymentAuth.InternalAPI.Payments.ProcessEffects
                                                ( processPaymentEffects )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(getPayment) )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant                        ( NoContent(..)
                                                , ServerError
                                                , err404
                                                , throwError
                                                )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( PaymentId )
import           Shared.WebAPI.PaymentAuth.API  ( TraceContext
                                                , UpdatePaymentBody(..)
                                                )

updateAPayment
  :: ( HasPaymentsDB m
     , HasDwollaClient m
     , HasLedgerDB m
     , HasRandom m
     , HasTime m
     , HasAccounts m
     , HasRiskScoresDB m
     , MonadIO m
     , MonadError ServerError m
     , HasEventTracking m
     , MonadMask m
     )
  => TraceContext
  -> PaymentId
  -> UpdatePaymentBody
  -> m NoContent
updateAPayment trace pid UpdatePaymentBody {..} = do
  tracePrint trace
             "updateAPayment "
             (pid, paymentStatus, paymentSetMethodId, paymentSetACHInfo)

  paymentM <- getPayment trace pid
  payment  <- case paymentM of
    Nothing -> throwError err404
    Just p  -> return p

  payEvts <- updatePayment trace
                           pid
                           paymentStatus
                           paymentSetMethodId
                           paymentSetACHInfo

  tracePrint trace "updateAPayment payEvts " (pid, payment, payEvts)

  mapM_ (processPaymentEffects trace) payEvts

  return NoContent
