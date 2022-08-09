import { useMemo } from "react";
import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";

import { State } from "../../State/State";
import {
  getTransactions,
  queryPath as trxQueryPath,
} from "../../Actions/User/GetTransactions";
import { PurchaseProps } from "../../Components/MainScreen/Purchases/Types";

const staleTimeMs = 30 * 1000; // 30 sec

export default function useGetTransaction(): {
  status: QueryStatus;
  data: PurchaseProps[] | undefined;
} {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const transactionQuery = useQuery([trxQueryPath], getTransactions(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  const data = useMemo(() => {
    let data = transactionQuery.data;
    if (data !== undefined) {
      data = data.filter((t) => t.amount !== "0.00");
    }
    return data;
  }, [transactionQuery.data]);

  return { status: transactionQuery.status, data: data };
}
