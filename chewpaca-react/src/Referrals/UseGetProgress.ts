import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgress } from "./Types";

export const path = "/referrals/progress/revisions/:pid";

export default function useGetProgress(
  pid: string,
  enabled?: boolean
): UseQueryResult<ReferralProgress[], unknown> {
  const realpath = path.replace(":pid", pid);

  return useQuery(
    [path, pid],
    async () => {
      const res = await axios.get<ReferralProgress[]>(
        `${serverlocation}${realpath}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { enabled }
  );
}
