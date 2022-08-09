{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.InternalAPI.Payments.ProcessEffects where

import           Data.Time                      ( UTCTime
                                                , defaultTimeLocale
                                                , formatTime
                                                )
import           PaymentAuth.App.Payments.Effects
                                                ( PaymentEffects(..) )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( (.=)
                                                , HasEventTracking(..)
                                                , object
                                                )
import           Shared.Models.Currency         ( currencyToDouble
                                                , getIsoCode
                                                , showCurr
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentStatus(..)
                                                , PaymentSubType(..)
                                                , PaymentType(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

timeFormatter :: UTCTime -> String
timeFormatter = formatTime defaultTimeLocale "%x"

processPaymentEffects
  :: (HasDwollaClient m, HasEventTracking m)
  => TraceContext
  -> PaymentEffects
  -> m ()
processPaymentEffects _ RiskWasUpdated{}      = return ()
processPaymentEffects _ LedgerWasUpdated{}    = return ()
processPaymentEffects t (PaymentWasCreated p) = initiatePayment t p
processPaymentEffects t (PaymentWasUpdated prev Payment { payStatus = st@PaymentPending, payUser = uid, ..})
  = do
    let direction = case payType of
          DebitFromUser -> "debit"
          CreditToUser  -> "credit"

    trackEventWithProps t uid "Payment Initiated" $ object
      [ "paymentId" .= payId
      , "revenue" .= currencyToDouble payAmount
      , "amount" .= showCurr payAmount
      , "currency" .= getIsoCode payAmount
      , "payment_date" .= timeFormatter payCreatedAt
      , ("direction", direction)
      , "visible" .= case paySubType of
        InitialVerification -> False
        RefundVerification  -> False
        NormalPayment       -> True
      , "subtype" .= show paySubType
      , "previousState" .= prev
      , "state" .= st
      ]

    return ()
processPaymentEffects t (PaymentWasUpdated prev Payment { payStatus = st@PaymentCompleted, payUser = uid, ..})
  = do
    let direction = case payType of
          DebitFromUser -> "debit"
          CreditToUser  -> "credit"

    trackEventWithProps t uid "Payment Completed" $ object
      [ "paymentId" .= payId
      , "revenue" .= currencyToDouble payAmount
      , "amount" .= showCurr payAmount
      , "currency" .= getIsoCode payAmount
      , "payment_date" .= timeFormatter payCreatedAt
      , ("direction", direction)
      , "visible" .= case paySubType of
        InitialVerification -> False
        RefundVerification  -> False
        NormalPayment       -> True
      , "subtype" .= show paySubType
      , "previousState" .= prev
      , "state" .= st
      ]

    return ()
processPaymentEffects t (PaymentWasUpdated prev Payment { payStatus = st@(PaymentFailed failReason), payUser = uid, ..})
  = do
    let direction = case payType of
          DebitFromUser -> "debit"
          CreditToUser  -> "credit"

    trackEventWithProps t uid "Payment Failed" $ object
      [ "paymentId" .= payId
      , "revenue" .= currencyToDouble payAmount
      , "amount" .= showCurr payAmount
      , "currency" .= getIsoCode payAmount
      , "payment_date" .= timeFormatter payCreatedAt
      , ("direction", direction)
      , "visible" .= case paySubType of
        InitialVerification -> False
        RefundVerification  -> False
        NormalPayment       -> True
      , "subtype" .= show paySubType
      , "previousState" .= prev
      , "state" .= st
      , "reason" .= failReason
      ]

    return ()
processPaymentEffects t (PaymentWasUpdated prev Payment { payStatus = st@PaymentCancelled, payUser = uid, ..})
  = do
    let direction = case payType of
          DebitFromUser -> "debit"
          CreditToUser  -> "credit"

    trackEventWithProps t uid "Payment Cancelled" $ object
      [ "paymentId" .= payId
      , "revenue" .= currencyToDouble payAmount
      , "amount" .= showCurr payAmount
      , "currency" .= getIsoCode payAmount
      , "payment_date" .= timeFormatter payCreatedAt
      , ("direction", direction)
      , "visible" .= case paySubType of
        InitialVerification -> False
        RefundVerification  -> False
        NormalPayment       -> True
      , "subtype" .= show paySubType
      , "previousState" .= prev
      , "state" .= st
      ]

    return ()
processPaymentEffects _ (PaymentWasUpdated _ Payment { payStatus = PaymentCreated })
  = return ()
processPaymentEffects t (PaymentWasCancelled Payment {..}) =
  cancelPayment t payId
