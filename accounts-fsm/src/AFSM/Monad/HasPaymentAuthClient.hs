module AFSM.Monad.HasPaymentAuthClient
  ( module AFSM.Monad.HasPaymentAuthClient
  , JournalType(..)
  , TraceContext(..)
  , MakeVerificationPaymentBody(..)
  , UpdatePaymentBody(..)
  , ClientError(..)
  , JournalSearch(..)
  , LedgerJournal(..)
  ) where

import           Data.Text                      ( Text )
import           Servant.Client                 ( ClientError(..) )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( JournalId
                                                , PaymentId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Common    ( LedgerFact )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , JournalType(..)
                                                , LedgerJournal(..)
                                                )
import           Shared.Models.Payment          ( Payment )
import           Shared.WebAPI.PaymentAuth.API  ( MakeVerificationPaymentBody(..)
                                                , TraceContext(..)
                                                , UpdatePaymentBody(..)
                                                )

class Monad m => HasPaymentAuthClient m where
  getSpendableBalance     :: TraceContext -> UserID -> m Currency
  makeVerificationPayment :: TraceContext -> UserID -> MakeVerificationPaymentBody -> m ()
  getPayment              :: TraceContext -> PaymentId -> m Payment
  updatePayment           :: TraceContext -> PaymentId -> UpdatePaymentBody -> m ()
  pingPaymentAuth         :: m ()
  createLedger            :: TraceContext -> JournalType -> Text -> m ()
  getLedger               :: TraceContext -> JournalSearch -> m [LedgerJournal]
  journalTransfer         :: TraceContext -> JournalId -> JournalId -> LedgerFact -> m ()

