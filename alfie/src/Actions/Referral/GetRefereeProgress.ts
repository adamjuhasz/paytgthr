import { useSelector } from "react-redux";
import { QueryResult, useQuery } from "react-query";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";
import { State } from "../../State/State";
import { ReferralProgress } from "./Types";

export const queryPath = "/app/referral/referee";

export const getRefereesProgress = (baseURL: string) => async (): Promise<
  ReferralProgress[]
> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as ReferralProgress[];
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

export const useGetRefereesProgress = (): QueryResult<
  ReferralProgress[],
  unknown
> => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const query = useQuery([queryPath], getRefereesProgress(baseURL));
  return query;
};
