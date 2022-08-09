import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";

export const queryPath = "/app/ledger/recent";

import { Ledger, LedgerFromServer } from "./Type";

export const getMostRecentLedger = (
  baseURL: string
) => async (): Promise<Ledger> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as LedgerFromServer;
      return {
        ...body,
        updated: new Date(body.updated),
        balance: body.balance[1] / body.balance[2],
      };
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};
