import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

import { path as getPath } from "./UseGetPayment";
import { path as userPath } from "./UseGetUserPayments";

export default function useCancelPayment(): UseMutationResult<
  unknown,
  unknown,
  string,
  unknown
> {
  const client = useQueryClient();

  const mutation = useMutation(
    async (payment: string) => {
      const res = await axios.post<unknown>(
        `${serverlocation}/payment/${payment}/cancel`,
        {
          responseType: "json",
        }
      );
      await client.invalidateQueries([getPath, payment]);
      return res;
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([userPath]);
      },
    }
  );

  return mutation;
}
