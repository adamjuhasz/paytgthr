module PaymentAuth.App.Purchases.GetTransaction where

import           Data.Text                      ( Text )
import           PaymentAuth.Monad.Transactions
                                               as M
                                                ( HasTransactionsDB(..) )
import           Shared.Models.Ids              ( TransactionId
                                                , UserID
                                                )
import           Shared.Models.Transaction      ( Transaction )
import           Shared.WebAPI.General.API      ( TraceContext )

getTransactionFromAptoId
  :: (HasTransactionsDB m) => TraceContext -> Text -> m (Maybe Transaction)
getTransactionFromAptoId = getTransactionUsingSourceId

getTransaction
  :: (HasTransactionsDB m)
  => TraceContext
  -> TransactionId
  -> m (Maybe Transaction)
getTransaction = M.getTransaction

getRecentTransactions
  :: (HasTransactionsDB m) => TraceContext -> UserID -> Int -> m [Transaction]
getRecentTransactions = getTransactionsFor
