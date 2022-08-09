import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TwoSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { SubmitLimiter, limitIsExceeded } from "../../Helpers/SubmitLimiter";

export const urlPath = "/app/user/requestStatement";
const limiter: SubmitLimiter = {
  firstAttempt: null,
  lastAttempt: null,
  attemptCount: 0,
};

export const submitRequestStatement = async (
  baseURL: string
): Promise<void> => {
  const limitUser = limitIsExceeded(limiter, 1);
  if (limitUser) {
    return;
  }

  const url = `${baseURL}${urlPath}`;
  const res = await retryFetch(TwoSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
  });

  switch (res.status) {
    case 200:
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`submitRequestStatement got status code ${res.status}`);
  }
};
