import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as journalsPath } from "./UseGetJournals";

interface Input {
  journalType: string;
  dwollaId: string;
}

export default function useChangeJournals(
  userid: string
): UseMutationResult<unknown, unknown, Input, unknown> {
  const queryClient = useQueryClient();

  const mutation = useMutation(
    ({ journalType, dwollaId }: Input) => {
      const form = new FormData();
      form.append("jtype", journalType);
      form.append("dwolla", dwollaId);

      return axios.post(
        `${serverlocation}/ledger/journals/user/${userid}`,
        form
      );
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([journalsPath, userid]);
      },
    }
  );

  return mutation;
}
