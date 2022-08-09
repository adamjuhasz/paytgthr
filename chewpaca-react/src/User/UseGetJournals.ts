import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Journal } from "../Ledger/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/ledger/journals";

export default function useGetJournals(
  user: string
): UseQueryResult<Journal[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<Journal[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
