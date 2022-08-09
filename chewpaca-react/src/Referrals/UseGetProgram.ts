import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgram } from "./Types";

export const path = "/referrals/programs/:pid";

export default function useGetProgram(
  pid: string,
  enabled?: boolean
): UseQueryResult<ReferralProgram, unknown> {
  const realpath = path.replace(":pid", pid);

  return useQuery(
    [path, pid],
    async () => {
      const res = await axios.get<{ program: ReferralProgram }>(
        `${serverlocation}${realpath}`,
        {
          responseType: "json",
        }
      );
      return res.data.program;
    },
    { enabled }
  );
}
