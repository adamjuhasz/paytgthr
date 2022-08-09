{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.Ledger.CreateFacts
  ( createAdjFact
  , createAdjFacts
  , createPaymentFact
  ) where

import           Data.Functor                   ( (<&>) )
import           PaymentAuth.App.Ledger.Update  ( FactTuple )
import           PaymentAuth.App.Split          ( calculateSplit )
import           Shared.Models.Currency         ( Currency(..)
                                                , getIsoCode
                                                , getMonetaryValue
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Ledger.Common    ( LedgerFact(..) )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentStatus(..)
                                                , PaymentType(..)
                                                )
import           Shared.Models.Transaction      ( AptoAdjustment(..)
                                                , AptoAdjustmentID(..)
                                                , TransactionId
                                                )

createAdjFact
  :: (TransactionId, [(UserID, Rational)]) -> AptoAdjustment -> [FactTuple]
createAdjFact (tid, trxSplitAmounts) AptoAdjustment { adjId = AptoAdjustmentID idem, ..}
  = calculateSplit theValue trxSplitAmounts
    <&> (\(user, valueOwed) ->
          (user, Just idem, TrxAdjustment tid (Currency theISO valueOwed))
        )
 where
  theValue = getMonetaryValue adjAmountBilling
  theISO   = getIsoCode adjAmountBilling

createAdjFacts
  :: (TransactionId, [(UserID, Rational)]) -> [AptoAdjustment] -> [FactTuple]
createAdjFacts trx adjs =
  let facts :: [[FactTuple]] = fmap (createAdjFact trx) adjs
  in  foldr (<>) [] facts

createPaymentFact :: Payment -> FactTuple
createPaymentFact Payment { payStatus = PaymentCompleted, payType = DebitFromUser, ..}
  = (payUser, payMethodId, PaymentCleared payId payAmount)
createPaymentFact Payment { payStatus = PaymentCompleted, payType = CreditToUser, payFromJournal = Just _, payToJournal = Just _, ..}
  = (payUser, payMethodId, PaymentCleared payId payAmount)
createPaymentFact Payment { payStatus = PaymentCompleted, payType = CreditToUser, ..}
  = (payUser, payMethodId, PaymentCleared payId (payAmount * (-1)))
createPaymentFact Payment { payStatus = PaymentFailed _, payType = DebitFromUser, ..}
  = (payUser, payMethodId, PaymentCleared payId (payAmount * (-1)))
createPaymentFact Payment { payStatus = PaymentFailed _, payType = CreditToUser, ..}
  = (payUser, payMethodId, PaymentCleared payId payAmount)
createPaymentFact Payment { payStatus = PaymentPending, ..} =
  error $ "Error: PaymentPending has no facts " <> show (payId, payUser)
createPaymentFact Payment { payStatus = PaymentCreated, ..} =
  error $ "Error: PaymentCreated has no facts " <> show (payId, payUser)
createPaymentFact Payment { payStatus = PaymentCancelled, ..} =
  error $ "Error: PaymentCancelled has no facts " <> show (payId, payUser)
