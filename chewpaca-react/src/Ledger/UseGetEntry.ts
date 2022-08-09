import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { LedgerEntry } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/ledger/entry/:entryid";

export default function useGetLedgerEntry(
  entry: string,
  enabled?: boolean
): UseQueryResult<LedgerEntry, unknown> {
  const realPath = path.replace(":entryid", entry);

  const query = useQuery(
    [path, entry],
    async () => {
      const res = await axios.get<{ entry: LedgerEntry }>(
        `${serverlocation}${realPath}`,
        {
          responseType: "json",
        }
      );
      return res.data.entry;
    },
    { enabled }
  );

  return query;
}
