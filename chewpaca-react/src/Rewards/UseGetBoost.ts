import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { RewardBoost } from "./Types";

export const path = "/rewards/reward/:rewardid";

export default function useGetBoost(
  rewardid: string
): UseQueryResult<RewardBoost, unknown> {
  const realpath = `${serverlocation}${path}`.replace(":rewardid", rewardid);
  console.log("realpath", realpath);

  const rewards = useQuery([path, rewardid], async () => {
    const res = await axios.get<RewardBoost>(realpath, {
      responseType: "json",
    });
    return res.data;
  });

  return rewards;
}
