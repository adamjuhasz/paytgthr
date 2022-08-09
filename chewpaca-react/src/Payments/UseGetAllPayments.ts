import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Payment } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/payments";

export default function useGetAllPayments(): UseQueryResult<
  Payment[],
  unknown
> {
  const query = useQuery(
    [path],
    async () => {
      const res = await axios.get<Payment[]>(`${serverlocation}${path}`, {
        responseType: "json",
      });
      return res.data;
    },
    { staleTime: 86400000 }
  );

  return query;
}
