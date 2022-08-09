import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { Group } from "../Group/Types";

export const path = "/user/:uid/groups";

export default function useGetGroups(
  user: string
): UseQueryResult<Group[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<Group[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
