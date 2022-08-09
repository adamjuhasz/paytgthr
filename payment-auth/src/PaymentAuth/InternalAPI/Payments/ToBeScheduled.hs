module PaymentAuth.InternalAPI.Payments.ToBeScheduled where

import           Control.Monad                  ( forM )
import           Data.Maybe                     ( fromJust )
import           PaymentAuth.App.CreatePayments ( addPayments )
import           PaymentAuth.App.Ledger.SchedulePayments
                                                ( getUsersWhoOweUs )
import           PaymentAuth.Monad.Accounts     ( HasAccounts(getUser) )
import           PaymentAuth.Monad.Ledger       ( HasLedgerDB )
import           PaymentAuth.Monad.Payments     ( HasPaymentsDB )
import           Shared.Models.Currency         ( getMonetaryValue )
import           Shared.Models.User             ( UserModel )
import           Shared.WebAPI.General.API      ( TraceContext )

usersNeedingScheduledPayments
  :: (HasLedgerDB m, HasAccounts m, HasPaymentsDB m)
  => TraceContext
  -> m [(UserModel, Double)]
usersNeedingScheduledPayments trace = do
  usersWithLedgers <- getUsersWhoOweUs trace
  forM usersWithLedgers $ \(uid, bal, payments) -> do
    user <- fromJust <$> getUser trace uid
    let totalOwed = bal + addPayments payments
    return (user, fromRational . getMonetaryValue $ totalOwed)
