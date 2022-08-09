module APIDwolla.InternalAPI.PaymentCancel where

import qualified APIDwolla.ACHTransfer.Cancel  as ACH
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient )
import           APIDwolla.Monad.Payment        ( HasPayments )
import           Control.Monad.IO.Class         ( MonadIO )
import           Servant                        ( NoContent(NoContent) )
import           Shared.Models.Ids              ( PaymentId )
import           Shared.WebAPI.General.API      ( TraceContext )

cancelPayment
  :: (HasDwollaClient m, HasPayments m, MonadIO m)
  => TraceContext
  -> PaymentId
  -> m NoContent
cancelPayment trace paymentId = do
  ACH.cancelTransfer trace paymentId
  return NoContent
