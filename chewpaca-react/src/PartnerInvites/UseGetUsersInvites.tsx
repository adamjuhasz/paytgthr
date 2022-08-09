import { UseQueryResult, useQuery } from "react-query";

import axios from "axios";

import { serverlocation } from "../Backend/Server";
import { PartnerInvite } from "./Types";

export const path = "/user/:uid/invites";

export default function useGetUsersInvites(
  userId: string,
  enabled?: boolean
): UseQueryResult<PartnerInvite[], unknown> {
  const result = useQuery(
    ["user", userId, "invites"],
    async () => {
      const res = await axios.get<PartnerInvite[]>(
        `${serverlocation.replace(":uid", userId)}${path}`,
        {
          responseType: "json",
        }
      );
      return res.data;
    },
    { enabled }
  );

  return result;
}
