import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/note";

interface Return {
  note: string;
}

export default function useGetNotes(
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
