import { UseQueryResult, useQuery } from "react-query";

import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { PartnerInvite } from "./Types";

export const path = "/invites";

export default function useGetAllInvites(
  enabled?: boolean
): UseQueryResult<PartnerInvite[], unknown> {
  const result = useQuery(
    ["invites"],
    async () => {
      const res = await axios.get<PartnerInvite[]>(`${serverlocation}${path}`, {
        responseType: "json",
      });
      return res.data;
    },
    { enabled }
  );

  return result;
}
