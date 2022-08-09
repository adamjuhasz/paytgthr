import { useState } from "react";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import console from "../../Global/Console";

import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { useAsync } from "../cancellablePromise";

import { formdataHeaders } from "../../Helpers/FormData";

export const submitLegalConsent = (
  baseURL: string
) => async (): Promise<void> => {
  const formdata = new FormData();
  formdata.append("doubleagree", "true");

  await Analytics.track("User LegalConsent Attempt");

  const url = `${baseURL}/app/signup/legalconsent`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("LegalConsent Agreed");
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: {},
      });
      throw new Error(`submitLegalConsent received status code ${res.status}`);
  }
};

type Return = { inProgress: boolean; submit: () => void; submitted: boolean };
export const useLegalConsent = (baseURL: string): Return => {
  const [inProgress, execute] = useAsync({});
  const [submitted, setSubmitted] = useState(false);

  const submit = async () =>
    execute(submitLegalConsent(baseURL)())
      .then(() => {
        setSubmitted(true);
      })
      .catch((err) => {
        console.error("error in useLegalConsent", err);
      });

  return { inProgress, submit, submitted };
};
