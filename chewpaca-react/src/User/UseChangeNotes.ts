import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { path as notesPath } from "./UseGetNotes";

export default function useAppendANote(
  userid: string
): UseMutationResult<unknown, unknown, string, unknown> {
  const queryClient = useQueryClient();

  const mutation = useMutation(
    (newNote: string) => {
      const form = new FormData();
      form.append("note", newNote);

      return axios.post(`${serverlocation}/user/${userid}/note/append`, form);
    },
    {
      onSuccess: async () => {
        await queryClient.invalidateQueries([notesPath, userid]);
      },
    }
  );

  return mutation;
}
