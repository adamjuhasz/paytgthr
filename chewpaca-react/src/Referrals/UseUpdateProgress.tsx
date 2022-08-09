import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { WorkflowProgress } from "./Types";

import { path } from "./UseGetProgress";

export interface Input {
  userid: string;
  progress: WorkflowProgress;
}

export default function useSetProgress(
  progressId: string
): UseMutationResult<unknown, unknown, Input, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (inp: Input) => {
      return axios.put<unknown>(
        `${serverlocation}/referrals/progress/${inp.userid}`,
        inp.progress,
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
