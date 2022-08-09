module APIDwolla.InternalAPI.ProcessChanges where

import qualified APIDwolla.Account.Create      as AC
import           APIDwolla.Monad.Payment        ( HasPayments(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Shared.Console
import           Shared.Models.User             ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , incrementTrace
                                                )
import           Shared.WebAPI.PaymentAuth.Client
                                                ( MakeVerificationPaymentBody(..)
                                                , VerificationPaymentStyle(..)
                                                )

processChanges
  :: (HasPayments m, MonadIO m)
  => TraceContext
  -> UserID
  -> AC.DwollaChanges
  -> m ()
processChanges _     _   AC.DwollaAccountCreated{} = return ()
processChanges trace uid (AC.DwollaFSCreated _)    = do
  newTrace <- incrementTrace trace

  let body = MakeVerificationPaymentBody UserSavedAmounts

  tracePrint trace "processChanges makeVerificationPayment " (uid, body)

  makeVerificationPayment newTrace uid body
