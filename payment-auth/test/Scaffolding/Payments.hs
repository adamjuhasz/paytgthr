module Scaffolding.Payments where

import qualified Data.UUID                     as U
import           Scaffolding.Users              ( userJohn )
import           Shared.Models.Currency         ( Currency(..) )
import           Shared.Models.Payment          ( Payment(..)
                                                , PaymentId(PaymentId)
                                                , PaymentMethod(..)
                                                , PaymentStatus(PaymentPending)
                                                , PaymentSubType(NormalPayment)
                                                , PaymentType
                                                  ( CreditToUser
                                                  , DebitFromUser
                                                  )
                                                )
import           Shared.Models.User             ( UserModel(usrUserID) )
import           Shared.TgthrMessages.Base      ( MessageID(MessageID) )
import           Shared.Utils                   ( stringToTime )

paymentAmount :: Currency
paymentAmount = Currency "USD" 50

paymentDebit :: Payment
paymentDebit = Payment
  { payId          = PaymentId U.nil
  , payRevision    = 1
  , payVersion     = "1.0"
  , payMsgSource   = MessageID U.nil
  , payStatus      = PaymentPending
  , payUser        = usrUserID userJohn
  , payType        = DebitFromUser
  , payMethod      = DwollaSettlement
  , payMethodId    = Nothing
  , payAmount      = paymentAmount
  , payText        = "Test"
  , paySubType     = NormalPayment
  , payACHInfo     = Nothing
  , payCreatedAt   = stringToTime "2019-11-01T20:15:32+00:00"
  , payFromJournal = Nothing
  , payToJournal   = Nothing
  }

paymentCredit :: Payment
paymentCredit = Payment
  { payId          = PaymentId U.nil
  , payRevision    = 1
  , payVersion     = "1.0"
  , payMsgSource   = MessageID U.nil
  , payStatus      = PaymentPending
  , payUser        = usrUserID userJohn
  , payType        = CreditToUser
  , payMethod      = DwollaSettlement
  , payMethodId    = Nothing
  , payAmount      = paymentAmount
  , payText        = "Test"
  , paySubType     = NormalPayment
  , payACHInfo     = Nothing
  , payCreatedAt   = stringToTime "2019-11-01T20:15:32+00:00"
  , payFromJournal = Nothing
  , payToJournal   = Nothing
  }

