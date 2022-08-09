import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { RiskScores } from "./Types";

import { serverlocation } from "../Backend/Server";

export const scoresPath = "/user/:uid/risk/scores";

export function useGetRiskScores(
  user: string
): UseQueryResult<RiskScores[], unknown> {
  const realPath = scoresPath.replace(":uid", user);

  const query = useQuery([scoresPath, user], async () => {
    const res = await axios.get<RiskScores[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}

export const scorePath = "/user/:uid/risk/score";

export function useGetRiskScore(
  user: string,
  enabled?: boolean
): UseQueryResult<RiskScores, unknown> {
  const realPath = scorePath.replace(":uid", user);

  const query = useQuery(
    [scorePath, user],
    async () => {
      const res = await axios.get<RiskScores>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled }
  );

  return query;
}
