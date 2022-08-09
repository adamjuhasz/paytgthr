import useGetJournalEntries from "./UseGetJournalEntries";
import LedgerEntriesFeed from "./EntriesFeed";
import { TimeInterval, filterDate } from "../Shared/IntervalFilter";

interface Props {
  journal: string;
  interval: TimeInterval;
}

export default function JournalFeed(props: Props): JSX.Element {
  const entries = useGetJournalEntries(props.journal);

  if (entries.status === "success") {
    const filteted = entries.data.filter((e) =>
      filterDate(e, "createdat", props.interval)
    );
    return <LedgerEntriesFeed entries={filteted} />;
  }

  return <div>Loading entries</div>;
}
