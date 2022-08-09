import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";

import { Payment, PaymentFromServer } from "./Type";

export const queryPath = "/app/payments/recent";

export const getRecentPayments = (baseURL: string) => async (): Promise<
  Payment[]
> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as PaymentFromServer[];
      return body.map((payment) => {
        return {
          ...payment,
          createdat: new Date(payment.createdat),
          amount: payment.amount[1] / payment.amount[2],
        };
      });
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};
