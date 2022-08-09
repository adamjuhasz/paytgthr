import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgress } from "./Types";

import { path } from "./UseGetProgress";

export default function useSetProgress(
  progressId: string
): UseMutationResult<unknown, unknown, ReferralProgress, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (prog: ReferralProgress) => {
      return axios.post<unknown>(
        `${serverlocation}/referrals/progress/${prog.progressId}`,
        prog,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([path, progressId]);
      },
    }
  );

  return mutation;
}
