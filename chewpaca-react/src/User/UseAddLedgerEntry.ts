import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as journalsPath } from "./UseGetJournals";
import { path as fullLedgerPath } from "./UseGetFullLedger";
import { path as entriesPath } from "../Ledger/UseGetJournalEntries";

interface Input {
  amount: string;
  from: string;
  to: string;
}

export default function useAddLedgerEntry(
  userid: string
): UseMutationResult<unknown, unknown, Input, unknown> {
  const queryClient = useQueryClient();

  const mutation = useMutation(
    (input: Input) => {
      const form = new FormData();
      form.append("amount", input.amount);
      form.append("from", input.from);
      form.append("to", input.to);

      return axios.post(
        `${serverlocation}/user/${userid}/ledger/adjustment`,
        form
      );
    },
    {
      onSuccess: async (_data, vars) => {
        await queryClient.invalidateQueries([journalsPath, userid]);
        await queryClient.invalidateQueries([fullLedgerPath, userid]);
        await queryClient.invalidateQueries([entriesPath, vars.from]);
        await queryClient.invalidateQueries([entriesPath, vars.to]);
      },
    }
  );

  return mutation;
}
