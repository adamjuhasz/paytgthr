import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as paymentsPath } from "../Payments/UseGetUserPayments";

export default function useDoLedgerPayment(
  userid: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  const mutation = useMutation(
    () => {
      return axios.post(
        `${serverlocation}/user/${userid}/payment/create/ledgerbalance`
      );
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([paymentsPath, userid]);
      },
    }
  );

  return mutation;
}
