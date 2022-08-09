module PaymentAuth.InternalAPI.User.Transactions where

import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Transaction      ( Transaction )
import           Shared.WebAPI.General.API      ( TraceContext )

getTransactions
  :: (HasTransactionsDB m) => TraceContext -> UserID -> Int -> m [Transaction]
getTransactions = getTransactionsFor
