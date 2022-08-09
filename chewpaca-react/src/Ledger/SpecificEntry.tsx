import { useParams } from "react-router";
import useGetLedgerEntry from "./UseGetEntry";

export const path = "/ledger/entry/:entry";

export default function SpecificTransaxction(_p: unknown): JSX.Element {
  const { entry } = useParams();
  const entryQuery = useGetLedgerEntry(entry || "", entry !== undefined);

  if (entry === undefined) {
    return <div>Missing entry id</div>;
  }

  if (entryQuery.status !== "success") {
    return <div>{entryQuery.status}...</div>;
  }

  return (
    <div>
      <pre>{JSON.stringify(entryQuery.data, null, 2)}</pre>
    </div>
  );
}
