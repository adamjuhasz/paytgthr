import { useState } from "react";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

type StateOfRequest = "none" | "inProgress" | "submited" | "error";

type Medium = "sms" | "email";

interface TokenRequestInfo {
  medium: Medium;
  identifier: string;
}

export const requestToken = (baseURL: string) => async ({
  medium,
  identifier,
}: TokenRequestInfo): Promise<void> => {
  const url = `${baseURL}/app/token/${medium}/create`;
  const formdata = new FormData();
  formdata.append("identifier", identifier);
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });
  switch (res.status) {
    case 200:
      await Analytics.track(`User CodeRequested ${medium}`, { identifier });
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`useRequestToken had a return of ${res.status}`);
  }
};

export const useRequestToken = (
  baseURL: string
): {
  state: StateOfRequest;
  submit: (medium: "sms" | "email", iden: string) => Promise<void>;
} => {
  const [state, setState] = useState<StateOfRequest>("none");

  const submit = async (medium: Medium, identifier: string) => {
    setState("inProgress");
    try {
      await requestToken(baseURL)({ medium, identifier });
      setState("submited");
    } catch (e) {
      setState("error");
    }
  };

  return { state, submit };
};
