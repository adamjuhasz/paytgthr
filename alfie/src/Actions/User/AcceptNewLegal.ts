import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

import { formdataHeaders } from "../../Helpers/FormData";

export const queryPath = "/app/user/legalconsent";

export const submitNewLegalConsent = (baseURL: string) => async (): Promise<
  void
> => {
  const formdata = new FormData();
  formdata.append("doubleagree", "true");

  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("UpdatedLegalConsent Agreed");
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(
        `submitNewLegalConsent received status code ${res.status}`
      );
  }
};
