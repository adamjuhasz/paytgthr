module APIDwolla.ACHTransfer.Create where

import           APIDwolla.Dwolla.Client        ( ClientErrors(..) )
import           APIDwolla.Monad.Accounts       ( HasAccounts(..) )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient(..) )
import           APIDwolla.Monad.Payment        ( HasPayments(..)
                                                , TraceContext
                                                )
import           Control.Monad.Reader           ( MonadIO(..) )
import           Data.Maybe                     ( fromJust )
import           Data.Text.Encoding             ( decodeUtf8 )
import           Shared.Console
import           Shared.Models.Payment          ( PaymentId(..)
                                                , PaymentStatus(..)
                                                )
import           Shared.Models.User             ( UserID
                                                , UserModel(..)
                                                )

createTransfer
  :: (HasAccounts m, HasDwollaClient m, HasPayments m, MonadIO m)
  => TraceContext
  -> UserID
  -> PaymentId
  -> m ()
createTransfer trace userId paymentId = do
  tracePrint trace "(updatePayment) Getting user, payment " (userId, paymentId)


  user    <- fromJust <$> getUser trace userId
  payment <- fromJust <$> getPayment trace paymentId

  res     <- createACHTransfer trace payment user

  let achInfo = case (usrBankRouting user, usrBankAcount user) of
        (Just r, Just a) -> Just (r, a)
        _                -> Nothing

  let cmd = updatePayment trace userId paymentId PaymentPending achInfo

  case res of
    Right loc                          -> cmd $ Just $ decodeUtf8 loc
    Left  (DwollaHTTPException reason) -> do
      traceError trace
                 "Error: DwollaHTTPException: "
                 (userId, paymentId, reason)
      cmd Nothing
    Left e -> do
      traceError trace
                 "Error: updatePayment case res of Left "
                 (userId, paymentId, e)
      cmd Nothing

  return ()
