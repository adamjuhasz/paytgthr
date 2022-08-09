import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";
import asyncPool from "tiny-async-pool";

import { serverlocation } from "../Backend/Server";
import { CloseReasons } from "../User/Types";

interface Input {
  users: string[];
  reason: CloseReasons;
}
export function useCloseUsers(): UseMutationResult<
  unknown,
  unknown,
  Input,
  unknown
> {
  const queryClient = useQueryClient();

  const mutation = useMutation(
    async ({ users, reason }: Input) => {
      const closeAUser = async (u: string) => {
        const realpath = "/user/:uid/state/close/:reason"
          .replace(":uid", u)
          .replace(":reason", reason);
        return axios.post(`${serverlocation}${realpath}`);
      };
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call
      const res = await asyncPool(3, users, closeAUser);
      // eslint-disable-next-line @typescript-eslint/no-unsafe-return
      return res;
    },
    {
      onSettled: async () => {
        await queryClient.invalidateQueries(["users"]);
      },
      onSuccess: async () => {
        await queryClient.invalidateQueries(["users"]);
      },
    }
  );

  return mutation;
}
