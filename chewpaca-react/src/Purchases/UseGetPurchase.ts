import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Purchase } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/transaction/:tid";

interface Return {
  purchase: Purchase;
  revisions: [Purchase, string][];
}

export default function useGetPurchase(
  transaction: string,
  enabled?: boolean
): UseQueryResult<Return, unknown> {
  const realpath = path.replace(":tid", transaction);

  const query = useQuery(
    [path, transaction],
    async () => {
      const res = await axios.get<Return>(`${serverlocation}${realpath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled }
  );

  return query;
}
