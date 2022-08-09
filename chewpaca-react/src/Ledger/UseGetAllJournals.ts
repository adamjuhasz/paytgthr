import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Journal } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/ledger/journals";

export default function useGetAllJournals(): UseQueryResult<
  Journal[],
  unknown
> {
  const query = useQuery([path], async () => {
    const res = await axios.get<Journal[]>(`${serverlocation}${path}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
