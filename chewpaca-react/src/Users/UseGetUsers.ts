import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { UserModel } from "../User/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/users";

export default function useGetUsers(
  enabled?: boolean,
  ids?: string[]
): UseQueryResult<UserModel[], unknown> {
  const query = useQuery(
    [path, ids],
    async () => {
      const qs = new URLSearchParams();
      if (ids !== undefined) {
        qs.set("ids", `[${ids.map((s) => `UserID ${s}`).join(",")}]`);
      }
      const res = await axios.get<UserModel[]>(
        `${serverlocation}${path}?${qs.toString()}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { enabled, staleTime: 1000 * 3600 }
  );

  return query;
}
