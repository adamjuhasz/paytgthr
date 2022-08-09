module PaymentAuth.InternalAPI.User.GetLiability where

import qualified PaymentAuth.App.GetBalance    as GB
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..) )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB(..) )
import           Shared.Models.Base             ( UserID )
import           Shared.Models.Currency         ( Currency )
import           Shared.WebAPI.General.API      ( TraceContext )

getLiability
  :: (HasLedgerDB m, HasTransactionsDB m, HasPaymentsDB m)
  => TraceContext
  -> UserID
  -> m Currency
getLiability = GB.getLiability
