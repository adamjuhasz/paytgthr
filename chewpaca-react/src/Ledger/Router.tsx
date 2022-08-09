import { Route, Routes } from "react-router-dom";

import AllJournalList, { path as journalsPAth } from "./AllJournalList";
import SpecificJournal, {
  path as specificJournalPath,
} from "./SpecificJournal";
import SpecificTransaction, { path as stPath } from "./SpecificTransaction";
import SpecificEntry, { path as entryPath } from "./SpecificEntry";
import StressTest, { path as stressPath } from "./StressTest";

export default function LedgerRouter(): JSX.Element {
  return (
    <Routes>
      <Route
        path={journalsPAth.replace("/ledger", "/")}
        element={<AllJournalList />}
      />
      <Route
        path={specificJournalPath.replace("/ledger", "/")}
        element={<SpecificJournal />}
      />
      <Route
        path={stPath.replace("/ledger", "/")}
        element={<SpecificTransaction />}
      />
      <Route
        path={entryPath.replace("/ledger", "/")}
        element={<SpecificEntry />}
      />
      <Route
        path={stressPath.replace("/ledger", "/")}
        element={<StressTest />}
      />
    </Routes>
  );
}
