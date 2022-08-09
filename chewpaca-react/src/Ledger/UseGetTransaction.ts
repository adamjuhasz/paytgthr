import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { LedgerTransaction } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/ledger/transaction/:tid";

export default function useGetLedgerTransaction(
  transaction: string,
  enabled?: boolean
): UseQueryResult<LedgerTransaction, unknown> {
  const realPath = path.replace(":tid", transaction);

  const query = useQuery(
    [path, transaction],
    async () => {
      const res = await axios.get<{ transaction: LedgerTransaction }>(
        `${serverlocation}${realPath}`,
        {
          responseType: "json",
        }
      );
      return res.data.transaction;
    },
    { enabled }
  );

  return query;
}
