module PaymentAuth.InternalAPI.Ledger.FindJournal where

import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           Shared.Models.Ledger.Journal   ( JournalSearch
                                                , LedgerJournal
                                                )
import           Shared.WebAPI.General.API      ( TraceContext )

findThisJournal
  :: (HasLedgerDB m) => TraceContext -> JournalSearch -> m [LedgerJournal]
findThisJournal = getLedgerJournalType
