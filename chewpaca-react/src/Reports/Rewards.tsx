import { useQuery } from "react-query";
import axios from "axios";
import { useParams } from "react-router-dom";

import { serverlocation } from "../Backend/Server";

export const path = "/reports/rewards/:interval/users";

export default function Reward(_props: unknown): JSX.Element {
  const { interval } = useParams();
  const realPath = path.replace(":interval", interval || "");

  const result = useQuery(
    ["reports", "rewards", interval, "users"],
    async (): Promise<Response> => {
      const res = await axios.get<Response>(`${serverlocation}${realPath}`, {
        responseType: "json",
      });
      return res.data;
    }
  );

  console.log(result);

  return <></>;
}
