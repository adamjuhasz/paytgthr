import { useSelector } from "react-redux";
import { useQuery } from "react-query";

import { State } from "../../State/State";
import {
  getInviteCode,
  getInviteCodePath,
} from "../../Actions/Invite/CodeBased";

const staleTime = 30 * 1000; // 30 sec

const useGetInvite = (): string | null => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const query = useQuery([getInviteCodePath], getInviteCode(baseURL), {
    staleTime: staleTime,
  });

  return query.data === undefined ? null : query.data.code;
};

export default useGetInvite;
