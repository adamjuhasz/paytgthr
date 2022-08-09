import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { LedgerEntry } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/ledger/entries";

export default function useGetFullLedger(
  user: string,
  enabled?: boolean
): UseQueryResult<LedgerEntry[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery(
    [path, user],
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
