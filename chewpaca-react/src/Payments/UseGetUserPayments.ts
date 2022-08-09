import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Payment } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/payments";

export default function useGetUserPayments(
  user: string
): UseQueryResult<Payment[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<Payment[]>(`${serverlocation}${realPath}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
