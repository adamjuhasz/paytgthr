module PaymentAuth.Plaid.RefreshAllBalances where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Functor                   ( (<&>) )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import           Data.Time.Clock                ( addUTCTime
                                                , nominalDay
                                                )
import           PaymentAuth.App.GetPlaidBalance
                                                ( getPlaidBalance )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(..) )
import           PaymentAuth.Monad.EventTracking
                                                ( HasEventTracking )
import           PaymentAuth.Monad.HttpClient   ( HasHttpClient )
import           PaymentAuth.Monad.Plaid        ( HasPlaidDB(..) )
import           PaymentAuth.Monad.RiskScores   ( HasRiskScoresDB )
import           PaymentAuth.Monad.Time        as MT
                                                ( HasTime(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.WebAPI.General.API      ( TraceContext )

refreshAllBalances
  :: ( HasAccounts m
     , HasTime m
     , HasPlaidDB m
     , HasHttpClient m
     , HasRiskScoresDB m
     , MonadIO m
     , HasEventTracking m
     )
  => TraceContext
  -> m Int
refreshAllBalances trace = do
  users <- getAllActiveUsers trace

  -- only for users who have had a balance in 48 hours
  now   <- MT.getCurrentTime
  let fortyEightHoursAgo = addUTCTime (nominalDay * (-2)) now
  let hasCache u =
        getUsersBalanceWithin u fortyEightHoursAgo <&> \r -> r <&> (u, )
  let getPlaid = getPlaidBalance trace

  usersWithCache <- mapM hasCache users <&> filter isJust <&> fmap
    (fst . fromJust)

  tracePrint
    trace
    "Info: (RefreshAllBalances) num of users: "
    (length users, length usersWithCache, length users - length usersWithCache)

  res <- mapM getPlaid usersWithCache
  let refreshed = filter isJust res

  tracePrint trace
             "Info: (RefreshAllBalances) refreshed count: "
             (length users, length usersWithCache, length refreshed)

  return $ length refreshed
