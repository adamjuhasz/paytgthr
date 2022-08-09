module PaymentAuth.Monad.Transactions where

import           Data.Text                      ( Text )
import           Shared.Models.Ids              ( TransactionId
                                                , UserID
                                                )
import           Shared.Models.Transaction      ( Transaction )
import           Shared.WebAPI.General.API      ( TraceContext )

class Monad m => HasTransactionsDB m where
  getTransaction              :: TraceContext -> TransactionId -> m (Maybe Transaction)
  saveTransaction             :: TraceContext -> Transaction -> m ()
  getPendingTransactionsFor   :: TraceContext -> UserID -> m [Transaction]
  getTransactionUsingSourceId :: TraceContext -> Text -> m (Maybe Transaction)
  getTransactionsFor          :: TraceContext -> UserID -> Int -> m [Transaction]
