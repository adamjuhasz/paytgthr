{-# LANGUAGE FlexibleContexts #-}

module PaymentAuth.InternalAPI.Purchase.Query where

import           Control.Monad.Except           ( MonadError(throwError) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Servant.Server                 ( ServerError(errBody)
                                                , err404
                                                )
import           Shared.Models.Ids              ( TransactionId )
import           Shared.Models.Transaction      ( Transaction(..)
                                                , TransactionSrcId
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

queryBySourceId
  :: (HasTransactionsDB m)
  => TraceContext
  -> TransactionSrcId
  -> m (Maybe Transaction)
queryBySourceId = getTransactionUsingSourceId

getATransaction
  :: (HasTransactionsDB m, MonadError ServerError m)
  => TraceContext
  -> TransactionId
  -> m Transaction
getATransaction trace tid = do
  trxMaybe <- getTransaction trace tid
  case trxMaybe of
    Nothing ->
      throwError err404 { errBody = "Journal Type already exists for user" }
    Just t -> return t
