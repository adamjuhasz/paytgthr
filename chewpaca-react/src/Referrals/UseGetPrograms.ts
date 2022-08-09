import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { ReferralProgram } from "./Types";

export const path = "/referrals/programs";

export default function useGetPrograms(): UseQueryResult<
  ReferralProgram[],
  unknown
> {
  return useQuery([path], async () => {
    const res = await axios.get<ReferralProgram[]>(`${serverlocation}${path}`, {
      responseType: "json",
    });
    return res.data;
  });
}
