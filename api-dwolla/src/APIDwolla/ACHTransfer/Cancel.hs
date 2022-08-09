module APIDwolla.ACHTransfer.Cancel where

import           APIDwolla.Monad.Dwolla         ( HasDwollaClient(..) )
import           APIDwolla.Monad.Payment        ( HasPayments(..)
                                                , TraceContext
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromJust )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Ids              ( PaymentId(..) )

cancelTransfer
  :: (HasDwollaClient m, HasPayments m, MonadIO m)
  => TraceContext
  -> PaymentId
  -> m ()
cancelTransfer trace paymentId = do
  tracePrint trace "cancelTransfer payment " paymentId

  payment <- fromJust <$> getPayment trace paymentId

  _       <- cancelACHTransfer trace payment

  return ()
