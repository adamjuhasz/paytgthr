import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  UnauthorizedError,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";
import { UserState } from "../../Types/UserStateTypes";

export const queryPath = "/app/user/state";

export const getCurrentUserState = (
  baseURL: string
) => async (): Promise<UserState> => {
  const url = `${baseURL}${queryPath}`;

  const res = await retryFetch(1000 * 5, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body: UserState = await res.json();

      // dates come in as strings
      const consentDate = body.user.consent as unknown;
      const disclosureDate = body.user.disclosure as unknown;
      body.user.consent =
        consentDate === null ? null : new Date(consentDate as string);
      body.user.disclosure =
        disclosureDate === null ? null : new Date(disclosureDate as string);

      return body;
    }

    case 401:
    case 403:
      await Analytics.track("Signed Out", {
        reason: "Session failure",
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new UnauthorizedError();

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getCurrentUserState got status of ${res.status}`);
  }
};
