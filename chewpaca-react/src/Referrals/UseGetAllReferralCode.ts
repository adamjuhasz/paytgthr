import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralCode } from "./Types";

export const path = "/referrals/codes";

export default function useGetReferralCode(
  enabled?: boolean
): UseQueryResult<ReferralCode[], unknown> {
  return useQuery(
    [path],
    async () => {
      const res = await axios.get<ReferralCode[]>(`${serverlocation}${path}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled }
  );
}
