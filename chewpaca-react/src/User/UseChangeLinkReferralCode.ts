import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

import { path } from "./UseGetReferralProgress";

export default function useChangeLinkReferralCode(
  user: string
): UseMutationResult<unknown, unknown, string, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (code: string) => {
      return axios.post<unknown>(
        `${serverlocation}/referrals/codes/${code}/link/${user}`,
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
