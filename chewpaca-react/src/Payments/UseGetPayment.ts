import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Payment } from "../Payments/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/payment/:pid";

interface Return {
  payment: Payment;
  revisions: Payment[];
}

export default function useGetPayment(
  payment: string,
  enabled?: boolean
): UseQueryResult<Return, unknown> {
  const realPath = path.replace(":pid", payment);

  const query = useQuery(
    [path, payment],
    async () => {
      const res = await axios.get<Return>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled }
  );

  return query;
}
