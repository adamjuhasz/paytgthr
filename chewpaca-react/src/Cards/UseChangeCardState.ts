import { UseMutationResult, useMutation, useQueryClient } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { CardStatus } from "./Types";

import { path } from "../User/UseGetCards";

interface Input {
  cardid: string;
  cardstate: CardStatus;
}

export default function useChangeCardState(
  userid: string
): UseMutationResult<unknown, unknown, Input, unknown> {
  const client = useQueryClient();

  const mutation = useMutation(
    async ({ cardid, cardstate }: Input) => {
      return axios.post<unknown>(
        `${serverlocation}/user/${userid}/card/${cardid}/state/${cardstate}`,
        {
          responseType: "json",
        }
      );
    },
    {
      onSuccess: async () => {
        await client.invalidateQueries([path, userid]);
      },
    }
  );

  return mutation;
}
