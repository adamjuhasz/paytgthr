/* global fetch */

import { store } from "../State/Store";
import console from "./Console";
import { logoutWithoutHistory } from "../Actions/Logout";

const ourFetch = async (
  input: RequestInfo,
  init: RequestInit
): Promise<Response> => {
  const token = store.getState().userInfo.token;
  const isLoggedIn = store.getState().userInfo.loggedIn;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const authHeaders: any = {};
  if (token !== null) {
    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
    authHeaders["Authorization"] = `Bearer ${token}`;
  }

  const incomingHeaders = init.headers !== undefined ? init.headers : {};
  const headers = { ...incomingHeaders, ...authHeaders };
  init.headers = headers;

  const startTime = new Date().getTime();
  const res = await fetch(input, { ...init });
  const endTime = new Date().getTime();

  console.log("Fetch", input, "status", res.status, `${endTime - startTime}ms`);

  switch (res.status) {
    case 200: {
      const newToken = res.headers.get("token");
      if (newToken !== null) {
        store.dispatch({ type: "setToken", token: newToken });
      }
      break;
    }

    case 401:
      if (isLoggedIn) {
        void logoutWithoutHistory(store.dispatch);
      }
      break;
  }

  return res;
};

export default ourFetch;
