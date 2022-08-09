import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";
import { defaultTo } from "lodash";

import { State } from "../../State/State";
import {
  Reward as AReward,
  getOurRewards,
  queryOurPath,
} from "../../Actions/Rewards/GetRewards";
export type Reward = AReward;

const staleTimeMs = 30 * 1000; // 30 sec

type Return = { status: QueryStatus; data: Reward[] };

export default function useGetOurRewards(): Return {
  const { baseURL, userid } = useSelector((state: State) => ({
    baseURL: state.baseURL,
    userid: state.userInfo.userId,
  }));

  const query = useQuery([queryOurPath], getOurRewards(baseURL), {
    refetchInterval: staleTimeMs,
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
    enabled: userid !== null,
  });

  return { status: query.status, data: defaultTo(query.data, []) };
}
