import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { Currency } from "../Shared/Types";
import { Payment } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/payments/scheduled";

export interface SchedulePayment {
  userid: string;
  ledger: Currency;
  payments: Payment[];
}

export default function useGetScheduledPayments(): UseQueryResult<
  SchedulePayment[],
  unknown
> {
  const query = useQuery([path], async () => {
    const res = await axios.get<SchedulePayment[]>(`${serverlocation}${path}`, {
      responseType: "json",
    });
    return res.data;
  });

  return query;
}
