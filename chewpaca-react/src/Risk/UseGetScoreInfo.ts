import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

export const path = "/risk/:score/limit";

interface Return {
  level: string;
  limit: [string, number, number];
  progressToNext: number;
  spendToNext: [string, number, number];
}

export default function useGetScoreInfo(
  score: number,
  enabled?: boolean
): UseQueryResult<Return, unknown> {
  const realPath = path.replace(":score", score.toFixed(1));

  const query = useQuery(
    [path, score],
    async () => {
      const res = await axios.get<Return>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled: enabled }
  );

  return query;
}
