import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { Currency, Purchase } from "../Shared/Types";

export const path = "/reports/weekly/global";

export interface Response {
  purchases: Purchase[];
  grouped: [string, Purchase[]][];
  sumPerGroup: [string, Currency][];
  totalMedian: Currency;
  totalMean: Currency;
  spentByReward: [string | null, Currency][];
}

export default function useGetWeeklyGlobal(): UseQueryResult<
  Response,
  unknown
> {
  const result = useQuery(
    ["group", "weekly", "report"],
    async (): Promise<Response> => {
      const res = await axios.get<Response>(`${serverlocation}${path}`, {
        responseType: "json",
      });
      return res.data;
    }
  );

  return result;
}
