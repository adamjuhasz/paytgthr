import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgram } from "./Types";

import { path } from "./UseGetPrograms";

export default function useNewProgram(): UseMutationResult<
  unknown,
  unknown,
  ReferralProgram,
  unknown
> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (newProgram: ReferralProgram) => {
      return axios.post<unknown>(
        `${serverlocation}/referrals/new/program`,
        newProgram,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([path]);
      },
    }
  );

  return mutation;
}
