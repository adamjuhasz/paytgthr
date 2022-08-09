import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Journal } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/ledger/journal/:jid";

export default function useGetJournal(
  journal: string,
  enabled?: boolean
): UseQueryResult<Journal, unknown> {
  const realPath = path.replace(":jid", journal);

  const query = useQuery(
    [path, journal],
    async () => {
      const res = await axios.get<Journal>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled }
  );

  return query;
}
