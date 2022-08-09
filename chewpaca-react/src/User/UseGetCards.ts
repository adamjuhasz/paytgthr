import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { DebitCard } from "../Cards/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/cards";

export default function useGetCards(
  user: string
): UseQueryResult<DebitCard[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<DebitCard[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
