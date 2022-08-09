import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";

import { State } from "../../State/State";
import {
  Entry as AnEntry,
  EntriesResponse,
  getEntries,
  queryEntries,
} from "../../Actions/Rewards/GetRewards";
export type Entry = AnEntry;

const staleTimeMs = 30 * 1000; // 30 sec

type Return =
  | { status: QueryStatus.Success; data: EntriesResponse }
  | {
      status: QueryStatus.Error | QueryStatus.Idle | QueryStatus.Loading;
      data: null;
    };

export default function useGetRewardEntries(): Return {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const query = useQuery([queryEntries], getEntries(baseURL), {
    refetchInterval: staleTimeMs,
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  if (query.status === QueryStatus.Success) {
    if (query.data === undefined) {
      return { status: QueryStatus.Error, data: null };
    } else {
      return { status: query.status, data: query.data };
    }
  } else {
    return { status: QueryStatus.Error, data: null };
  }
}
