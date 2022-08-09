module PaymentAuth.App.Payments.GetPayment where

import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           PaymentAuth.Monad.Payments    as M
                                                ( HasPaymentsDB(..) )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

getPayment
  :: (HasPaymentsDB m) => TraceContext -> PaymentId -> m (Maybe Payment)
getPayment = M.getPayment

getPaymentFromSourceId
  :: (HasPaymentsDB m) => TraceContext -> Text -> m (Maybe Payment)
getPaymentFromSourceId = M.getPaymentFromSourceId

getPaymentsCreatedAtTime
  :: (HasPaymentsDB m) => TraceContext -> PaymentId -> m (Maybe UTCTime)
getPaymentsCreatedAtTime = M.getPaymentsCreatedAtTime
