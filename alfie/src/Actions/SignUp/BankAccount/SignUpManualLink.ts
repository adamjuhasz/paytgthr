import { useState } from "react";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";
import { queryCache } from "react-query";

import { useAsync } from "../../cancellablePromise";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../../fetchRequests";
import { SubmitLimiter, limitIsExceeded } from "../../../Helpers/SubmitLimiter";
import { queryPath as stateQueryPath } from "../../User/GetUserState";
import { formdataHeaders } from "../../../Helpers/FormData";

const limiter: SubmitLimiter = {
  firstAttempt: null,
  lastAttempt: null,
  attemptCount: 0,
};

export const submitManualDetails = (baseURL: string) => async (
  accountNumber: string,
  routingNumber: string,
  accountName: string,
  bankName: string
): Promise<void> => {
  const limitUser = limitIsExceeded(limiter, 3);
  if (limitUser) {
    void Analytics.track("ManualLink Entry Error", {
      error: "limited",
      bankName,
      accountName,
      routingNumber,
    });
    const error: SubmitErrors = {
      metaError: "Attempts",
    };
    throw error;
  }

  const formdata = new FormData();
  formdata.append("bank", bankName);
  formdata.append("name", accountName);
  formdata.append("routing", routingNumber);
  formdata.append("accountNum", accountNumber);

  const url = `${baseURL}/app/signup/manuallink/entry`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("ManualLink Entry", {
        bankName,
        accountName,
        routingNumber,
      });
      await Analytics.track("FundingSource Linked");
      return;

    case 400:
      // bad entry
      await Analytics.track("ManualLink Entry Error", {
        error: "format",
        bankName,
        accountName,
        routingNumber,
      });
      throw new Error(`submitManualDetails had a return of ${res.status}`);

    case 403:
      await Analytics.track("ManualLink Entry Error", {
        error: "AlreadyLinked",
        bankName,
        accountName,
        routingNumber,
      });
      throw new Error(`submitManualDetails had a return of ${res.status}`);

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`submitManualDetails had a return of ${res.status}`);
  }
};

export interface SubmitErrors {
  metaError: "None" | "Attempts";
}

type Return = {
  inProgress: boolean;
  submit: (
    accountNumber: string,
    routingNumber: string,
    accountName: string,
    bankName: string
  ) => void;
  errors: SubmitErrors;
  submitted: boolean;
};
export const useSubmitManual = (baseURL: string): Return => {
  const initialError: SubmitErrors = { metaError: "None" };
  const [inProgress, execute, errors] = useAsync(initialError);
  const [submitted, setSubmitted] = useState(false);

  const submiter = (
    accountNumber: string,
    routingNumber: string,
    accountName: string,
    bankName: string
  ) => {
    const action = submitManualDetails(baseURL)(
      accountNumber,
      routingNumber,
      accountName,
      bankName
    );
    execute(action)
      .then(() => {
        queryCache.removeQueries([stateQueryPath]);
        setSubmitted(true);
      })
      .catch(() => undefined);
  };

  return { inProgress, submit: submiter, errors, submitted };
};
