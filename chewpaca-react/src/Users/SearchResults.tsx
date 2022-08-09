import { useParams, useSearchParams } from "react-router-dom";

import useSearch from "./UseSearch";
import UserTable from "./UserTable";

export const path = "/users/search/:type";

export default function SearchResults(): JSX.Element {
  const [searchParams] = useSearchParams();
  const params = useParams();
  const q = searchParams.get("q");
  const type = params.type;

  const search = useSearch(type || "unknown", q || "unknown");

  if (q === null) {
    return <div>q search param is missing</div>;
  }

  if (type === undefined) {
    return <div>type param is missing</div>;
  }

  if (search.status === "success") {
    return (
      <div className="p-2">
        <UserTable users={search.data} />
      </div>
    );
  }

  return <div className="p-2">Serching...</div>;
}
