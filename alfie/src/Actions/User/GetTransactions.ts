import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import { PurchaseProps } from "../../Components/MainScreen/Purchases/Types";
import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";

export type UserLevels =
  | "level0"
  | "level1"
  | "level2"
  | "level3"
  | "level4"
  | "level5"
  | "level6"
  | "level7"
  | "level8"
  | "level9"
  | "level10";

interface APIResponseV1 {
  transactions: PurchaseProps[];
  version: 1;
}

export const queryPath = "/app/transactions";

export const getTransactions = (baseURL: string) => async (): Promise<
  PurchaseProps[]
> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as APIResponseV1;
      const purchases = body.transactions.filter(
        (p) => p.description !== "Digital Wallet Action"
      );

      return purchases;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};
