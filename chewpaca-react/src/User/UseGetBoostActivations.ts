import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { RewardBoostActivation } from "../Rewards/Types";

export const path = "/rewards/activations/:gid";

export default function useGetBoostsActivations(
  group: string,
  enabled?: boolean
): UseQueryResult<RewardBoostActivation[], unknown> {
  const realPath = path.replace(":gid", group);

  const query = useQuery(
    [path, group],
    async () => {
      const res = await axios.get<RewardBoostActivation[]>(
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
