import { Route, Routes } from "react-router-dom";

import NewToday from "./NewToday";
import AppEventUsers, { path as appUsersPath } from "./AppEvents";
import UsersSearch, { path as usersSearchPath } from "./Search";
import SearchResults, { path as usersSearchResultsPath } from "./SearchResults";
import BrowseUsers, { path as browsePath } from "./Browse";

export default function UsersRouter(): JSX.Element {
  return (
    <Routes>
      <Route
        path={"/users/today".replace("/users", "/")}
        element={<NewToday />}
      />
      <Route
        path={appUsersPath.replace("/users", "/")}
        element={<AppEventUsers />}
      />
      <Route
        path={usersSearchPath.replace("/users", "/")}
        element={<UsersSearch />}
      />
      <Route
        path={usersSearchResultsPath.replace("/users", "/")}
        element={<SearchResults />}
      />
      <Route
        path={browsePath.replace("/users", "/")}
        element={<BrowseUsers />}
      />
    </Routes>
  );
}
