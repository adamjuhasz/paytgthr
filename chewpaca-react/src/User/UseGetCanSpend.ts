import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/canspend";

interface Return {
  canSpend: [string, number, number];
}

export default function useGetCanSpend(
  user: string,
  enabled?: boolean
): UseQueryResult<Return, unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery(
    [path, user],
    async () => {
      const res = await axios.get<Return>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled: enabled }
  );

  return query;
}
