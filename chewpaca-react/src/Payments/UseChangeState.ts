import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

import { path as getPath } from "./UseGetPayment";
import { path as allPath } from "./UseGetAllPayments";
import { path as schedPath } from "./UseGetScheduledPayments";

export default function useChangePaymentState(
  payment: string
): UseMutationResult<unknown, unknown, string, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (newstate: string) => {
      return axios.post<unknown>(
        `${serverlocation}/payment/${payment}/state/${newstate}`,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([getPath, payment]);
        await client.invalidateQueries([allPath]);
        await client.invalidateQueries([schedPath]);
      },
    }
  );

  return mutation;
}
