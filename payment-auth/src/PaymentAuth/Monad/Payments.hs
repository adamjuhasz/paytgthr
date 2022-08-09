module PaymentAuth.Monad.Payments where

import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           Shared.Models.Base             ( UserID )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

data IncludeVerificationPayments
  = IncludeVerification
  | WithoutVerifcication
  deriving (Eq, Show)

class Monad m => HasPaymentsDB m where
  getPayment               :: TraceContext -> PaymentId -> m (Maybe Payment)
  savePayment              :: TraceContext -> Payment -> m ()
  getPendingPaymentsOf     :: TraceContext -> UserID -> IncludeVerificationPayments -> m [Payment]
  getPaymentFromSourceId   :: TraceContext -> Text -> m (Maybe Payment)
  getPaymentsCreatedAtTime :: TraceContext -> PaymentId -> m (Maybe UTCTime)
  getPaymentsOf            :: TraceContext -> UserID -> m [Payment]

