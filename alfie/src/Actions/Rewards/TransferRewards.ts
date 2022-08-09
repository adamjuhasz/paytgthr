import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { formdataHeaders } from "../../Helpers/FormData";

export const transferReward = (baseURL: string) => async (
  amount: number
): Promise<void> => {
  const formdata = new FormData();
  formdata.append("amount", amount.toFixed(2));

  const url = `${baseURL}/app/rewards/payout/trasfer`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Rewards Transfered", { amount });
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { amount },
      });
      throw new Error(`transferReward had return of ${res.status}`);
  }
};
