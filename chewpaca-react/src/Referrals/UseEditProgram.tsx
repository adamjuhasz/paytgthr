import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgram } from "./Types";

import { path as allProgramPath } from "./UseGetPrograms";
import { path as singleProgramPath } from "./UseGetProgram";

export default function useEditProgram(
  pid: string
): UseMutationResult<unknown, unknown, ReferralProgram, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (newProgram: ReferralProgram) => {
      return axios.post<unknown>(
        `${serverlocation}/referrals/programs/${pid}`,
        newProgram,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([allProgramPath]);
        await client.invalidateQueries([singleProgramPath, pid]);
      },
    }
  );

  return mutation;
}
