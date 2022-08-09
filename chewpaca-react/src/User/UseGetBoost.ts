import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { RewardBoost } from "../Rewards/Types";

export const path = "/rewards/boosts/:gid";

export default function useGetBoosts(
  group: string,
  enabled?: boolean
): UseQueryResult<RewardBoost[], unknown> {
  const realPath = path.replace(":gid", group);

  const rewards = useQuery(
    [path, group],
    async () => {
      const res = await axios.get<RewardBoost[]>(
        `${serverlocation}${realPath}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { enabled: enabled }
  );

  return rewards;
}
