module PaymentAuth.App.Ledger.SchedulePayments
  ( schedulePaymentsFromLedger
  , getUsersWhoOweUs
  ) where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import           PaymentAuth.App.CreatePayments ( addPayments )
import           PaymentAuth.App.Payments.Effects
                                                ( PaymentEffects(..) )
import           PaymentAuth.App.Payments.MakeAPayment
                                                ( payOutLedger )
import           PaymentAuth.Monad.Ledger      as L
                                                ( HasLedgerDB(..) )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB(..)
                                                , IncludeVerificationPayments(..)
                                                )
import           PaymentAuth.Monad.Random       ( HasRandom(..) )
import           PaymentAuth.Monad.Time         ( HasTime(..) )
import           Shared.Console                 ( tracePrint )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( UserID )
import           Shared.Models.Payment          ( Payment(..) )
import           Shared.WebAPI.General.API      ( TraceContext )

collectPendingPayments
  :: (HasPaymentsDB m)
  => TraceContext
  -> (UserID, a)
  -> m (UserID, a, [Payment])
collectPendingPayments trace (user, balance) =
  (user, balance, ) <$> getPendingPaymentsOf trace user WithoutVerifcication

reduceUsersOwesUs
  :: [(UserID, Currency, [Payment])] -> [(UserID, Currency, [Payment])]
reduceUsersOwesUs = filter (\(_, c, pays) -> (c + addPayments pays) < 0)

getUsersWhoOweUs
  :: (HasLedgerDB m, HasPaymentsDB m)
  => TraceContext
  -> m [(UserID, Currency, [Payment])]
getUsersWhoOweUs trace = do
  usersWithBalance <- getUsersWithBalances trace
  withPayments     <- mapM (collectPendingPayments trace) usersWithBalance

  return $ reduceUsersOwesUs withPayments

schedulePaymentsFromLedger
  :: (HasLedgerDB m, HasPaymentsDB m, HasRandom m, MonadIO m, HasTime m)
  => TraceContext
  -> m [PaymentEffects]
schedulePaymentsFromLedger trace = do
  owesUs <- getUsersWhoOweUs trace

  liftIO $ mapM_
    (tracePrint trace "schedulePaymentsFromLedger getUsersWhoOweUs ")
    owesUs

  let users = fmap (\(u, _, _) -> u) owesUs

  evts <- mapM (payOutLedger trace) users

  liftIO $ mapM_
    (tracePrint trace "schedulePaymentsFromLedger finalPayments ")
    (zip owesUs evts)

  return $ concat evts

