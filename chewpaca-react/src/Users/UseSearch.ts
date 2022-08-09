import { UseQueryResult, useQuery } from "react-query";
import axios from "axios";
import { UserModel } from "../User/Types";

import { serverlocation } from "../Backend/Server";

export const path = "/users/search/:type";

export default function useSearch(
  type: string,
  search: string
): UseQueryResult<UserModel[], unknown> {
  const realPath = path.replace(":type", type);

  const formData = new FormData();
  formData.append("q", search);

  const query = useQuery([path, type, search], async () => {
    const res = await axios.get<UserModel[]>(
      `${serverlocation}${realPath}?q=${search}`,
      {
        responseType: "json",
      }
    );
    return res.data;
  });

  return query;
}
