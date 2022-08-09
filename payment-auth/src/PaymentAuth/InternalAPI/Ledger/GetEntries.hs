module PaymentAuth.InternalAPI.Ledger.GetEntries where

import           PaymentAuth.Monad.Ledger       ( HasLedgerDB(..) )
import           Shared.Models.Ids              ( JournalId(..) )
import           Shared.Models.Ledger.Entry     ( LedgerEntry(..) )
import           Shared.WebAPI.General.API      ( TraceContext(..) )

getEntries :: (HasLedgerDB m) => TraceContext -> JournalId -> m [LedgerEntry]
getEntries = getEntriesForLedger
