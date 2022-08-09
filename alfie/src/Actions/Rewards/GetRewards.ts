import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

export interface Reward {
  boostName: string;
  boostRewardInBips: number;
  boostCreated: string;
  boostId: string;
  boostMaximumPayout: [string, number, number];
  boostActive: boolean;
  boostUpdated: string;
  boostExpiresInHr: number;
  boostUses: null | number;
}

export const queryAllPath = "/app/rewards/all";
export const getAllRewards = (baseURL: string) => async (): Promise<
  Reward[]
> => {
  const url = `${baseURL}${queryAllPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as Reward[];
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};

export const queryOurPath = "/app/rewards/ours";
export const getOurRewards = (baseURL: string) => async (): Promise<
  Reward[]
> => {
  const url = `${baseURL}${queryOurPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as Reward[];
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};

export interface Entry {
  revision: number;
  transaction: string;
  amount: [string, number, number];
  description: string | null;
  reward: string | null;
}

export interface EntriesResponse {
  balance: [string, number, number];
  entries: Entry[];
}

export const queryEntries = "/app/rewards/entries";
export const getEntries = (
  baseURL: string
) => async (): Promise<EntriesResponse> => {
  const url = `${baseURL}${queryEntries}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as EntriesResponse;
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};
