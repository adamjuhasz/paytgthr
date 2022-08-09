import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

import { path } from "./UseGetReferralCode";

export default function useChangeReferralCode(
  user: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async () => {
      return axios.post<unknown>(
        `${serverlocation}/referrals/new/code/${user}`,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([path, user]);
      },
    }
  );

  return mutation;
}
