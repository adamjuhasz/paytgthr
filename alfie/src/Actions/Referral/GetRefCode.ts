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

export const queryPath = "/app/referral/code";

export interface Return {
  code: string;
  created: string;
}

export const getReferralCode = (
  baseURL: string
) => async (): Promise<Return> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as Return;
      void Analytics.track("User ReferralCode Requested", body);
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

export const useGetReferralCode = (): QueryResult<Return, unknown> => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const query = useQuery([queryPath], getReferralCode(baseURL));
  return query;
};
