module PaymentAuth.Monad.Dwolla where

import           Shared.Models.Ids              ( PaymentId )
import           Shared.Models.Payment          ( Payment )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasDwollaClient m where
  initiatePayment :: TraceContext -> Payment -> m ()
  cancelPayment   :: TraceContext -> PaymentId -> m ()
