import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgress } from "../Referrals/Types";

export const path = "/referrals/progress/self/:uid";

export default function useGetReferralProgress(
  user: string,
  enabled?: boolean
): UseQueryResult<ReferralProgress, unknown> {
  const realpath = path.replace(":uid", user);

  return useQuery(
    [path, user],
    async () => {
      const res = await axios.get<{ progress: ReferralProgress }>(
        `${serverlocation}${realpath}`,
        {
          responseType: "json",
        }
      );
      return res.data.progress;
    },
    { enabled }
  );
}
