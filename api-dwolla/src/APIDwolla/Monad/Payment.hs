module APIDwolla.Monad.Payment
  ( HasPayments(..)
  , TraceContext(..)
  , MakeVerificationPaymentBody(..)
  ) where

import           Data.Text                      ( Text )
import           Shared.Models.Payment          ( Payment
                                                , PaymentId
                                                , PaymentStatus
                                                )
import           Shared.Models.User             ( RedactedText
                                                , UserID
                                                )
import           Shared.WebAPI.General.API      ( TraceContext(..) )
import           Shared.WebAPI.PaymentAuth.API  ( MakeVerificationPaymentBody(..)
                                                )

class Monad m => HasPayments m where
  getPayment              :: TraceContext -> PaymentId -> m (Maybe Payment)
  updatePayment           :: TraceContext -> UserID -> PaymentId ->  PaymentStatus -> Maybe (RedactedText, RedactedText) -> Maybe Text -> m ()
  makeVerificationPayment :: TraceContext -> UserID -> MakeVerificationPaymentBody -> m ()
