module PaymentAuth.InternalAPI.User.GetSpendableBalance where

import           Control.Monad.IO.Class         ( MonadIO )
import qualified PaymentAuth.App.GetBalance    as App
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Transactions ( HasTransactionsDB )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( UserID )
import           Shared.WebAPI.General.API      ( TraceContext )

getSpendbaleBalance
  :: ( HasLedgerDB m
     , HasTransactionsDB m
     , HasPaymentsDB m
     , HasRiskScoresDB m
     , MonadIO m
     )
  => TraceContext
  -> UserID
  -> m Currency
getSpendbaleBalance trace uid = do
  App.getSpendingLimit trace uid
