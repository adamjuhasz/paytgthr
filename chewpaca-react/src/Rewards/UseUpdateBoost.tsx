import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { RewardBoost } from "./Types";

import { path as allBoostsPath } from "./UseGetBoosts";
import { path as singleBoostsPath } from "./UseGetBoost";

export default function useUpdateBoost(
  rewardid: string
): UseMutationResult<unknown, unknown, RewardBoost, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (updatedBoost: RewardBoost) => {
      return axios.put<unknown>(
        `${serverlocation}/rewards/reward/${rewardid}`,
        updatedBoost,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([allBoostsPath]);
        await client.invalidateQueries([singleBoostsPath, rewardid]);
      },
    }
  );

  return mutation;
}
