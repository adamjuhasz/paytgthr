module PaymentAuth.InternalAPI.Admin.InitiatePaymentsFromLedger where

import           Control.Monad.IO.Class         ( MonadIO )
import           PaymentAuth.App.Ledger.SchedulePayments
                                                ( schedulePaymentsFromLedger )
import           PaymentAuth.InternalAPI.Payments.ProcessEffects
                                                ( processPaymentEffects )
import           PaymentAuth.Monad.Dwolla       ( HasDwollaClient )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.Random       ( HasRandom )
import           PaymentAuth.Monad.Time         ( HasTime )
import           Servant.API                    ( NoContent(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

initiatePaymentsFromLedger
  :: ( HasLedgerDB m
     , HasEventTracking m
     , HasDwollaClient m
     , HasPaymentsDB m
     , HasRandom m
     , MonadIO m
     , HasTime m
     )
  => TraceContext
  -> m NoContent
initiatePaymentsFromLedger trace = do
  events <- schedulePaymentsFromLedger trace
  mapM_ (processPaymentEffects trace) events
  return NoContent
