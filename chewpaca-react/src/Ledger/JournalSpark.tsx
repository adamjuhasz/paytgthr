import { Link } from "react-router-dom";
import useGetJournal from "./UseGetJournal";

interface Props {
  journal: string;
}

export default function JournalSpark(props: Props): JSX.Element {
  const journal = useGetJournal(props.journal);

  if (journal.status === "success") {
    return (
      <Link
        to={`/ledger/journals/${props.journal}`}
        className="text-indigo-500 underline"
      >
        {journal.data.journalType.tag} ({props.journal.split("-")[0]})
      </Link>
    );
  }

  return (
    <Link
      to={`/ledger/journals/${props.journal}`}
      className="text-indigo-500 underline"
    >
      Journal: {props.journal.split("-")[0]}
    </Link>
  );
}
