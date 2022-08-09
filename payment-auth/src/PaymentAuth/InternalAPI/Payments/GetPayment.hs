{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PaymentAuth.InternalAPI.Payments.GetPayment where

import           Control.Monad.Except           ( MonadError(throwError) )
import qualified Data.ByteString.Lazy.Char8    as C
import           Data.Text                      ( Text )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           Servant.Server                 ( ServerError(..)
                                                , err404
                                                )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Payment          ( Payment(payStatus)
                                                , PaymentId
                                                , PaymentStatus
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

getPaymentFromDB
  :: (HasPaymentsDB m, MonadError ServerError m)
  => TraceContext
  -> PaymentId
  -> m Payment
getPaymentFromDB trace pid = do
  payment <- getPayment trace pid
  case payment of
    Just p  -> return p
    Nothing -> throwError err404
      { errBody = C.pack $ "Could not find payment " <> show pid
      }

getPayments
  :: (HasPaymentsDB m)
  => TraceContext
  -> UserID
  -> [PaymentStatus]
  -> m [Payment]
getPayments trace uid statuses = do
  payments <- getPaymentsOf trace uid
  let filtered = filter (\p -> payStatus p `elem` statuses) payments
  return filtered

getFromSourceId
  :: (HasPaymentsDB m, MonadError ServerError m)
  => TraceContext
  -> Text
  -> m Payment
getFromSourceId trace sourceId = do
  payment <- getPaymentFromSourceId trace sourceId
  case payment of
    Nothing -> throwError $ err404
      { errBody = C.pack $ "Could not find payment " <> show sourceId
      }
    Just pay -> return pay
