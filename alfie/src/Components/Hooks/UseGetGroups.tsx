import { useSelector } from "react-redux";
import { QueryStatus, useQuery } from "react-query";
import { GroupModelWithUser } from "../../Actions/Groups/GetUserGroups";

import { State } from "../../State/State";
import {
  getUsersGroups,
  queryPath as groupQueryPath,
} from "../../Actions/Groups/GetUserGroups";

const staleTime = 30 * 1000;

export default function useGetGroups(): {
  status: QueryStatus;
  data?: GroupModelWithUser[];
} {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const groupQuery = useQuery([groupQueryPath], getUsersGroups(baseURL), {
    staleTime: staleTime,
    refetchOnMount: false,
    refetchOnWindowFocus: false,
  });

  return { status: groupQuery.status, data: groupQuery.data };
}
