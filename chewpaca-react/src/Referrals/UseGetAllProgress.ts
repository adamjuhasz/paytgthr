import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgress } from "./Types";

export const path = "/referrals/progress";

export default function useGetAllProgresses(
  enabled?: boolean
): UseQueryResult<ReferralProgress[], unknown> {
  return useQuery(
    [path],
    async () => {
      const res = await axios.get<ReferralProgress[]>(
        `${serverlocation}${path}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { enabled, staleTime: 5 * 60 }
  );
}
