import { UseQueryResult, useQuery } from "react-query";

import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { Group } from "./Types";
export const path = "/groups";

export default function useGetGroups(
  enabled?: boolean,
  ids?: string[]
): UseQueryResult<Group[], unknown> {
  const result = useQuery(
    ["groups", ids],
    async () => {
      const qs = new URLSearchParams();
      if (ids !== undefined) {
        qs.set("ids", `[${ids.map((s) => `GroupId ${s}`).join(",")}]`);
      }

      const res = await axios.get<Group[]>(
        `${serverlocation}${path}?${qs.toString()}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { cacheTime: 1000 * 60 * 60, enabled }
  );

  return result;
}
