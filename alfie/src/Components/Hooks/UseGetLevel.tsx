import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";

import { State } from "../../State/State";
import {
  UserLevelResponse,
  getUserLevel,
  queryPath,
} from "../../Actions/User/GetUserLevel";

const staleTimeMs = 30 * 1000; // 30 sec

type Return =
  | { status: QueryStatus.Success; data: UserLevelResponse }
  | {
      status: QueryStatus.Error | QueryStatus.Idle | QueryStatus.Loading;
      data: UserLevelResponse;
    };

export default function useGetLevel(): Return {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const theQuery = useQuery([queryPath], getUserLevel(baseURL), {
    staleTime: staleTimeMs,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  const defaultData: UserLevelResponse = {
    currentLevel: 1,
    currentProgress: -1,
    levels: [],
    maxSpend: 150,
    canSpend: -1,
    hasSpent: 0,
    dollarsToNextLevel: 75,
    version: 1.0,
  };

  switch (theQuery.status) {
    case QueryStatus.Error:
    case QueryStatus.Idle:
    case QueryStatus.Loading:
      return { status: theQuery.status, data: defaultData };

    case QueryStatus.Success:
      if (theQuery.data !== undefined) {
        return { status: theQuery.status, data: theQuery.data };
      } else {
        return { status: QueryStatus.Error, data: defaultData };
      }
  }
}
