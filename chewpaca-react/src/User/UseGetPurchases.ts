import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Purchase } from "../Purchases/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/purchases";

export default function useGetPurchases(
  user: string
): UseQueryResult<Purchase[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<Purchase[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
