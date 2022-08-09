import { MutationResultPair, useMutation } from "react-query";
import { useSelector } from "react-redux";

import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { State } from "../../State/State";

export const path = "/app/referral/link";

export const linkRefferalCode = (baseURL: string) => async (
  code: string
): Promise<void> => {
  const url = `${baseURL}${path}`;

  const formdata = new FormData();
  formdata.append("code", code.toUpperCase().trim().replace("-", ""));

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as { groupid: string };
      await Analytics.track("User ReferralCode Linked", {
        group: body.groupid,
      });
      return;
    }

    case 404:
      await Analytics.track("User ReferralCode LinkError", {
        reason: "Code not found",
        code: code,
      });
      throw new Error("Code not found");

    case 409:
      await Analytics.track("User ReferralCode LinkError", {
        reason: "Already have a progress",
        code: code,
      });
      throw new Error("Already used a code");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { code },
      });
      throw new Error("unknwon error");
  }
};

export const useLinkCode = (): MutationResultPair<
  void,
  unknown,
  string,
  unknown
> => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const mutate = useMutation(linkRefferalCode(baseURL), { throwOnError: true });
  return mutate;
};
