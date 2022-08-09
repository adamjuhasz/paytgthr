import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { KYCAssesment } from "./Types";

import { serverlocation } from "../Backend/Server";

export const path = "/user/:uid/kyc/assesments";

export default function useGetNotes(
  user: string
): UseQueryResult<KYCAssesment[], unknown> {
  const realPath = path.replace(":uid", user);

  const query = useQuery([path, user], async () => {
    const res = await axios.get<KYCAssesment[]>(
      `${serverlocation}${realPath}`,
      {
        responseType: "json",
      }
    );
    return res.data;
  });

  return query;
}
