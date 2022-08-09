module PaymentAuth.InternalAPI.Admin.RefreshPlaidBalances where

import           Control.Monad.IO.Class         ( MonadIO )
import           PaymentAuth.Monad.Accounts     ( HasAccounts )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.HttpClient   ( HasHttpClient )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time         ( HasTime )
import           PaymentAuth.Plaid.RefreshAllBalances
                                                ( refreshAllBalances )
import           Servant.API                    ( NoContent(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

refreshPlaidBalances
  :: ( HasAccounts m
     , HasTime m
     , HasPlaidDB m
     , HasHttpClient m
     , HasRiskScoresDB m
     , MonadIO m
     , HasEventTracking m
     )
  => TraceContext
  -> m NoContent
refreshPlaidBalances trace = do
  _ <- refreshAllBalances trace
  return NoContent
