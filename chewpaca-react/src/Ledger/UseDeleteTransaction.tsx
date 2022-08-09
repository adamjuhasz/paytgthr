import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as trxPath } from "./UseGetTransaction";

export default function useDeleteJournalTransaction(
  transaction: string
): UseMutationResult<unknown, unknown, void, unknown> {
  const queryClient = useQueryClient();

  return useMutation(
    () => {
      return axios.delete(
        `${serverlocation}/ledger/transaction/${transaction}`
      );
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([trxPath, transaction]);
      },
    }
  );
}
