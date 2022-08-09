import { useQuery } from "react-query";
import { useSelector } from "react-redux";

import {
  getCurrentUserState,
  queryPath as stateQueryPath,
} from "../../Actions/User/GetUserState";
import { State as GlobalState } from "../../State/State";
import { UserState } from "../../Types/UserStateTypes";

export const queryPath = stateQueryPath;

const staleTime = 30 * 1000;

const useGetUser = (): UserState | null => {
  const { baseURL, userId } = useSelector((state: GlobalState) => ({
    baseURL: state.baseURL,
    userId: state.userInfo.userId,
  }));

  const stateQuery = useQuery([stateQueryPath], getCurrentUserState(baseURL), {
    staleTime: staleTime,
    enabled: userId !== null,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  if (stateQuery.status !== "success" || stateQuery.data === undefined) {
    return null;
  }

  return stateQuery.data;
};

export default useGetUser;
