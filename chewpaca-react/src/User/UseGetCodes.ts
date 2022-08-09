import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { UserCode } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/codes";

export default function useGetCodes(
  user: string
): UseQueryResult<UserCode[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<UserCode[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
