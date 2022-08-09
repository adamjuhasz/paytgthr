import { Link } from "react-router-dom";

import { path } from "./UserView";

import useGetUser from "../User/UseGetUserModel";

interface Props {
  userid: string;
  thisUser?: string;
}

export default function UserLink({ userid, thisUser }: Props): JSX.Element {
  const query = useGetUser(userid);

  return userid === thisUser ? (
    <span>self</span>
  ) : (
    <Link
      to={path.replace(":uid", userid)}
      className=" text-indigo-600 hover:text-indigo-900"
    >
      {query.status === "success" ? (
        `${query.data.firstname || ""} ${query.data.lastname || ""} (${
          userid.split("-")[0]
        })`.trim()
      ) : (
        <abbr key={userid} title={userid}>
          {userid.split("-")[0]}
        </abbr>
      )}
    </Link>
  );
}
