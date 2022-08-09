import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path } from "../User/UseGetBoostActivations";

export default function useCancelBoost(
  group?: string
): UseMutationResult<unknown, unknown, string, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (activation: string) => {
      return axios.post<unknown>(
        `${serverlocation}/rewards/activation/${activation}/cancel`,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        if (group !== undefined) {
          await client.invalidateQueries([path, group]);
        }
      },
    }
  );

  return mutation;
}
