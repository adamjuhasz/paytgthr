{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.Payments.PayOutLedger where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.Payments.MakeAPayment
                                                ( payOutLedger )
import           PaymentAuth.InternalAPI.Payments.ProcessEffects
                                                ( processPaymentEffects )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant                        ( NoContent(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )

payLedger
  :: ( HasPaymentsDB m
     , HasEventTracking m
     , HasDwollaClient m
     , HasLedgerDB m
     , HasRandom m
     , HasTime m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> m NoContent
payLedger trace uid = do
  evts <- payOutLedger trace uid
  mapM_ (processPaymentEffects trace) evts

  return NoContent
