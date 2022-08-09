import { UseQueryResult, useQuery } from "react-query";

import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { Purchase } from "../Shared/Types";

export const path = "/transactions";

export default function useGetPurchases(
  start?: Date,
  state?: ("TrxCompleted" | "TrxAuthorized" | "TrxPending")[]
): UseQueryResult<Purchase[], unknown> {
  const result = useQuery(
    [
      "transactions",
      start === undefined ? "all" : start.toISOString(),
      state === undefined ? "all" : state.sort().join(","),
    ],
    async (): Promise<Purchase[]> => {
      const qs = new URLSearchParams();
      if (start !== undefined) {
        qs.set("start", start.toISOString());
      }
      if (state !== undefined && state.length > 0) {
        qs.set("state", `[${state.join(",")}]`);
      }
      const res = await axios.get<Purchase[]>(
        `${serverlocation}${path}?${qs.toString()}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { cacheTime: 1000 * 60 * 60 }
  );

  return result;
}
