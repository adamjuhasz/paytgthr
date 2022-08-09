import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { UserModel } from "./Types";

export const path = "/user/:uid";

export default function useGetUserModel(
  user: string,
  enabled?: boolean
): UseQueryResult<UserModel, unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery(
    [path, user],
    async () => {
      const res = await axios.get<UserModel>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled: enabled }
  );

  return query;
}
