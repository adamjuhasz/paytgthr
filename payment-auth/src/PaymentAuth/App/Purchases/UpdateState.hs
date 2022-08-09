module PaymentAuth.App.Purchases.UpdateState where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Shared.Models.Ids              ( TransactionId )
import           Shared.Models.Transaction      ( Transaction
                                                  ( trxMsgSource
                                                  , trxRevision
                                                  , trxState
                                                  )
                                                , TransactionState
                                                )
import           Shared.WebAPI.General.API      ( TraceContext
                                                , traceToMID
                                                )

data UpdateTransactionStateErrors = CantFindTransaction
  deriving Show
instance Exception UpdateTransactionStateErrors

setTransactionState
  :: (HasTransactionsDB m)
  => TraceContext
  -> TransactionId
  -> TransactionState
  -> m ()
setTransactionState trace transactionId newState = do
  trxM <- getTransaction trace transactionId
  trx  <- case trxM of
    Nothing -> throw CantFindTransaction
    Just t  -> return t
  let newTrx = trx { trxState     = newState
                   , trxRevision  = trxRevision trx + 1
                   , trxMsgSource = traceToMID trace
                   }
  saveTransaction trace newTrx
  return ()
