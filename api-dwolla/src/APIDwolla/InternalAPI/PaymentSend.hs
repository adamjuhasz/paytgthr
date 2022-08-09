{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module APIDwolla.InternalAPI.PaymentSend where

import qualified APIDwolla.ACHTransfer.Create  as ACH
import           APIDwolla.Monad.Accounts       ( HasAccounts )
import           APIDwolla.Monad.Dwolla         ( HasDwollaClient )
import           APIDwolla.Monad.Payment        ( HasPayments(..) )
import           Control.Monad.Except           ( MonadError(throwError) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Servant                        ( NoContent(NoContent) )
import           Servant.Server                 ( ServerError
                                                , err404
                                                )
import           Shared.Console
import           Shared.Models.Payment          ( Payment(payUser)
                                                , PaymentId
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

sendPayment
  :: ( HasAccounts m
     , HasDwollaClient m
     , HasPayments m
     , MonadIO m
     , MonadError ServerError m
     )
  => TraceContext
  -> PaymentId
  -> m NoContent
sendPayment trace paymentId = do
  tracePrint trace "sendPayment " paymentId

  paymentM <- getPayment trace paymentId
  payment  <- case paymentM of
    Just p  -> return p
    Nothing -> do
      traceError trace "Error: sendPayment payment missing " paymentId
      throwError err404
  let userId = payUser payment

  tracePrint trace "sendPayment create transfer " (paymentId, userId, payment)

  ACH.createTransfer trace userId paymentId

  return NoContent
