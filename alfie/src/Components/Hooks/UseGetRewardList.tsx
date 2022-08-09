import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";
import { defaultTo } from "lodash";

import { State } from "../../State/State";
import {
  Reward as AReward,
  getAllRewards,
  queryAllPath,
} from "../../Actions/Rewards/GetRewards";
export type Reward = AReward;

const staleTimeMs = 60 * 1000; // 1 min

type Return = { status: QueryStatus; data: Reward[] };

export default function useGetRewardList(): Return {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const query = useQuery([queryAllPath], getAllRewards(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  return { status: query.status, data: defaultTo(query.data, []) };
}
