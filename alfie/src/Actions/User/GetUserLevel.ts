import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TwoSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";

type LevelNumber = number;
type LevelLimit = number;
type PurchasesTillNext = number;
export type Level = [LevelNumber, LevelLimit, PurchasesTillNext];

export interface LevelData {
  number: number;
  limitUSD: number;
  purchasesTillNext: number;
  name: string;
}

export interface UserLevelResponse {
  currentLevel: number;
  currentProgress: number;
  levels: LevelData[];
  maxSpend: number;
  canSpend: number;
  hasSpent: number;
  dollarsToNextLevel: number;
  version: number;
}

export const queryPath = "/app/user/level";

export const getUserLevel = (
  baseURL: string
) => async (): Promise<UserLevelResponse> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TwoSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as UserLevelResponse;
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: {},
      });
      throw new Error(`getUserLevel had return status of ${res.status}`);
  }
};
