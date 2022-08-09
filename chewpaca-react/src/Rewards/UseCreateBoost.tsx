import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { RewardBoost } from "./Types";

import { path as allBoostsPath } from "./UseGetBoosts";

export default function useCreateNewBoost(): UseMutationResult<
  unknown,
  unknown,
  RewardBoost,
  unknown
> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (newBoost: RewardBoost) => {
      return axios.post<unknown>(`${serverlocation}/rewards/new`, newBoost, {
        responseType: "json",
      });
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([allBoostsPath]);
      },
    }
  );

  return mutation;
}
