{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.Payments.CancelPayment where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad.Catch            ( MonadMask )
import           Control.Monad.IO.Class         ( MonadIO )
import           PaymentAuth.App.Payments.Effects
                                                ( PaymentEffects(..) )
import           PaymentAuth.App.Payments.UpdatePayment
                                                ( updatePayment )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentId
                                                , PaymentStatus(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data CancelPaymentErrors = CantFindPayment | PaymentCantBeCancelled PaymentStatus
  deriving Show
instance Exception CancelPaymentErrors

cancelAPayment
  :: ( HasPaymentsDB m
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
  -> m [PaymentEffects]
cancelAPayment trace paymentId = do
  payment <- getPayment trace paymentId
  case payment of
    Nothing -> throw CantFindPayment
    Just Payment { payStatus = PaymentPending, payMethodId = Nothing, ..} ->
      -- stuck in a bad state so cancel manually
      updatePayment trace payId PaymentCancelled Nothing Nothing
    Just p@Payment { payStatus = PaymentPending, payMethodId = Just _ } ->
      -- reset using Dwolla
      return [PaymentWasCancelled p]
    Just Payment { payStatus = PaymentCreated, ..} ->
      -- stuck in a bad state so cancel manually
      updatePayment trace payId PaymentCancelled payMethodId Nothing
    Just Payment { payStatus = PaymentCompleted } ->
      throw $ PaymentCantBeCancelled PaymentCompleted
    Just Payment { payStatus = PaymentFailed r } ->
      throw $ PaymentCantBeCancelled $ PaymentFailed r
    Just Payment { payStatus = PaymentCancelled } ->
      throw $ PaymentCantBeCancelled PaymentCancelled
