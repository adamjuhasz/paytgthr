{-# LANGUAGE RecordWildCards #-}

module PaymentAuth.App.CreatePayments where

import           Data.Time.Clock                ( UTCTime )
import           Data.UUID                      ( UUID )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( JournalId
                                                , PaymentId(..)
                                                , UserID
                                                )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentMethod(..)
                                                , PaymentStatus(..)
                                                , PaymentSubType(..)
                                                , PaymentType(..)
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

signedPayAmount :: Payment -> Currency
signedPayAmount Payment { payType = CreditToUser, ..}  = payAmount * (-1)
signedPayAmount Payment { payType = DebitFromUser, ..} = payAmount

addPayments :: [Payment] -> Currency
addPayments pays = sum (fmap signedPayAmount pays)

diffBalance :: (UserID, Currency, [Payment]) -> (UserID, Currency)
diffBalance (user, balance, pays) = (user, balance + addPayments pays)

newtype FromJournal = FromJournal JournalId
newtype ToJournal = ToJournal JournalId

createAPayment
  :: TraceContext
  -> UTCTime
  -> ( UserID
     , FromJournal
     , ToJournal
     , Currency
     , PaymentType
     , PaymentSubType
     )
  -> UUID
  -> Payment
createAPayment trace now (user, FromJournal fromJournal, ToJournal toJournal, amount, paymentType, subtype) i
  = Payment { payId          = PaymentId i
            , payRevision    = 1
            , payVersion     = "1.0"
            , payMsgSource   = traceToMID trace
            , payStatus      = PaymentCreated
            , payUser        = user
            , payType        = paymentType
            , payMethod      = DwollaSettlement
            , payMethodId    = Nothing
            , payAmount      = if amount > 0 then amount else amount * (-1)
            , payText        = "Tgthr Card"
            , paySubType     = subtype
            , payACHInfo     = Nothing
            , payCreatedAt   = now
            , payFromJournal = Just fromJournal
            , payToJournal   = Just toJournal
            }
