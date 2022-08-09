import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralCode } from "../Referrals/Types";

export const path = "/referrals/codes/user/:uid";

export default function useGetReferralCode(
  user: string,
  enabled?: boolean
): UseQueryResult<ReferralCode | null, unknown> {
  const realpath = path.replace(":uid", user);

  return useQuery(
    [path, user],
    async () => {
      const res = await axios.get<{ code: ReferralCode | null }>(
        `${serverlocation}${realpath}`,
        {
          responseType: "json",
        }
      );
      return res.data.code;
    },
    { enabled }
  );
}
