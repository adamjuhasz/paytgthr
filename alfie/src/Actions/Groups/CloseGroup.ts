import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

interface Return {
  group: string;
}

export const path = (id: string): string => `/app/group/${id}/close`;

export const closeGroup = (baseURL: string) => async (
  id: string
): Promise<Return> => {
  const url = `${baseURL}${path(id)}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, "Content-Type": "application/json" },
    body: JSON.stringify({}),
  });

  switch (res.status) {
    case 200: {
      const returnBody = (await res.json()) as Return;
      await Analytics.track("Group Closed", { group: id });
      return returnBody;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`Error with closeGroup, got code ${res.status}`);
  }
};
