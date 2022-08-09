import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { formdataHeaders } from "../../Helpers/FormData";

export const activateBoost = (baseURL: string) => async (
  boostId: string
): Promise<string> => {
  const formdata = new FormData();
  formdata.append("rewardid", boostId);

  const url = `${baseURL}/app/rewards/activate`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Boost Activated", { boostId });
      return boostId;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { boostId },
      });
      throw new Error(`activateBoost had return of ${res.status}`);
  }
};
