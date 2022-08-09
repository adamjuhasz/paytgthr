import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { RewardBoost } from "./Types";

export const path = "/rewards/list";

export default function useGetBoosts(
  enabled?: boolean
): UseQueryResult<RewardBoost[], unknown> {
  const rewards = useQuery(
    [path],
    async () => {
      const res = await axios.get<RewardBoost[]>(`${serverlocation}${path}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled: enabled }
  );

  return rewards;
}
