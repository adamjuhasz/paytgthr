import { Link, useParams, useSearchParams } from "react-router-dom";

import useGetJournal from "./UseGetJournal";
import JournalFeed from "./JournalFeed";
import DescriptionList, { Row } from "../Shared/DescriptionList";
import { currToStr } from "../Shared/Currency";
import UserLink from "../User/UserLink";
import { TimeInterval } from "../Shared/IntervalFilter";

export const path = "/ledger/journals/:jid";

export default function SpecificJournal(_p: unknown): JSX.Element {
  const { jid } = useParams();
  const [searchParams] = useSearchParams();
  const journal = useGetJournal(jid || "", jid !== undefined);

  if (jid === undefined) {
    return <div>No journal id found</div>;
  }

  if (journal.status !== "success") {
    return <div>Loading journal</div>;
  }

  console.log(searchParams);

  const timeInterval: TimeInterval =
    (searchParams.get("interval") as TimeInterval) ||
    (journal.data.journalRevision > 100 ? "30 days" : "All");

  return (
    <div className="m-2">
      <DescriptionList
        header={journal.data.journalName}
        subHeader={journal.data.journalId}
      >
        <Row
          left="User"
          clipboard={journal.data.journalUser || undefined}
          right={
            journal.data.journalUser === null ? (
              "<None>"
            ) : (
              <UserLink userid={journal.data.journalUser} />
            )
          }
        />
        <Row
          clipboard={currToStr(journal.data.journalBalance)}
          left="Balance"
          right={currToStr(journal.data.journalBalance)}
        />
        <Row
          left="Pending Balance"
          right={currToStr(journal.data.journalPendingBalance)}
        />
        <Row
          left="Created"
          right={new Date(journal.data.journalCreated).toLocaleString()}
        />

        <Row left="Type" right={journal.data.journalType.tag} />
        <Row left="Last revision" right={journal.data.journalRevision} />
        <Row
          left="Last updated"
          right={new Date(journal.data.journalUpdated).toLocaleString()}
        />
        <Row
          left="Last JTrx"
          clipboard={journal.data.journalTransaction}
          right={
            <Link
              to={`/ledger/transaction/${journal.data.journalTransaction}`}
              className="text-indigo-500 underline"
            >
              {journal.data.journalTransaction}
            </Link>
          }
        />
        <Row
          left="Last entry"
          clipboard={journal.data.lastJournalEntry}
          right={
            <Link
              to={`/ledger/entry/${journal.data.journalTransaction}`}
              className="text-indigo-500 underline"
            >
              {journal.data.lastJournalEntry}
            </Link>
          }
        />
      </DescriptionList>
      <h2 className="text-lg leading-6 font-medium text-gray-900 my-4">
        Showing time interval: "{timeInterval}"
      </h2>
      <JournalFeed journal={jid} interval={timeInterval} />
      <hr />
    </div>
  );
}
