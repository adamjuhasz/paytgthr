module PaymentAuth.Monad.Ledger
  ( module PaymentAuth.Monad.Ledger
  , CreateLedgerJournalBody(..)
  , JournalSearch(..)
  ) where

import           Data.Text                      ( Text )
import           Shared.Models.Currency         ( Currency )
import           Shared.Models.Ids              ( JournalId
                                                , LedgerEntryId
                                                , UserID
                                                )
import           Shared.Models.Ledger.Entry     ( LedgerEntry )
import           Shared.Models.Ledger.Journal   ( JournalSearch(..)
                                                , LedgerJournal
                                                )
import           Shared.Models.Ledger.Transaction
                                                ( LedgerTransaction )
import           Shared.WebAPI.General.API      ( TraceContext )
import           Shared.WebAPI.PaymentAuth.API  ( CreateLedgerJournalBody(..) )

class Monad m => HasLedgerDB m where
  getUsersWithBalances  :: TraceContext -> m [(UserID, Currency)]
  getLedgerForIdem      :: TraceContext -> JournalId -> Text -> m (Maybe LedgerEntry)
  saveLedgerTransaction :: TraceContext -> LedgerTransaction -> Maybe CreateLedgerJournalBody -> m ()
  getLedgerJournalType  :: TraceContext -> JournalSearch -> m [LedgerJournal]
  getLedgerJournal      :: TraceContext -> JournalId -> m (Maybe LedgerJournal )
  getLedgerEntry        :: TraceContext -> LedgerEntryId -> m (Maybe LedgerEntry)
  getEntriesForLedger   :: TraceContext -> JournalId -> m [LedgerEntry]
