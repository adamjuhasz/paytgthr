import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { LedgerEntry } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/ledger/journal/:jid/entries";

export default function useGetJournalEntries(
  journal: string,
  enabled?: boolean
): UseQueryResult<LedgerEntry[], unknown> {
  const realPath = path.replace(":jid", journal);

  const query = useQuery(
    [path, journal],
    async () => {
      const res = await axios.get<LedgerEntry[]>(
        `${serverlocation}${realPath}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { enabled: enabled }
  );

  return query;
}
