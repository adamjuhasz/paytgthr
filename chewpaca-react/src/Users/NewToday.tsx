import { useQuery } from "react-query";
import axios from "axios";

import { serverlocation } from "../Backend/Server";
import UserTable from "./UserTable";
import { UserModel } from "../User/Types";

export default function GlobalWeeklyReport(): JSX.Element {
  const result = useQuery(["users", "today"], async () => {
    const res = await axios.get<UserModel[]>(`${serverlocation}/users/today`, {
      responseType: "json",
    });
    return res.data;
  });

  if (result.status === "success") {
    console.log(result.data);
    return (
      <div className="w-full p-x-2">
        <UserTable users={result.data} />
      </div>
    );
  }

  return <></>;
}
