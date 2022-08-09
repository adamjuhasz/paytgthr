import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";

import { State } from "../../State/State";
import { getRecentPayments, queryPath } from "../../Actions/Payments/GetRecent";
import { Payment } from "../../Actions/Payments/Type";

const staleTimeMs = 30 * 1000; // 30 sec

export default function useGetPayments(): {
  status: QueryStatus;
  data: Payment[] | undefined;
} {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const paymentsQuery = useQuery([queryPath], getRecentPayments(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  return { status: paymentsQuery.status, data: paymentsQuery.data };
}
