import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { UserModel } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/revisions";

type Return = [UserModel, string][];

export default function useGetRevisions(
  user: string
): UseQueryResult<Return, unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<Return>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
