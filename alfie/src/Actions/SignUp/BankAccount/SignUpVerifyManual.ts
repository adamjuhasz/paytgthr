import { useState } from "react";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";
import { queryCache } from "react-query";

import { useAsync } from "../../cancellablePromise";
import {
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

type Errors = "NoVerifyError" | "BadAmount" | "BadFormat";
interface ErrorReturn {
  errors: [Errors];
}

export const submitAmount = (baseURL: string) => async (
  amount: string
): Promise<void> => {
  const limitUser = limitIsExceeded(limiter);
  if (limitUser) {
    const error: VerifyErrors = {
      amountError: "Attempts",
    };
    throw error;
  }

  const formdata = new FormData();
  formdata.append("amount1", amount);

  await Analytics.track("ManualLink Verify Attempt", { amount: amount });

  const url = `${baseURL}/app/signup/manuallink/verify`;
  const res = await retryFetch(1000 * 10, url, {
    ...commonOptions,
    method: "POST",
    headers: { ...commonPostHeaders, ...formdataHeaders(formdata) },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("ManualLink Verify");
      await Analytics.track("FundingSource Verified");
      return;

    case 400: {
      const body = (await res.json()) as ErrorReturn; // VerifyResponse

      const errors: VerifyErrors = {
        amountError: body.errors[0] === "BadAmount" ? "Amount" : "Format",
      };

      await Analytics.track("ManualLink Verify Error", {
        httpStatus: res.status,
        httpUrl: url,
        ...errors,
        submitedAmount: amount,
      });

      throw errors;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`submitPII had a return of ${res.status}`);
  }
};

export type AmountError = "None" | "Amount" | "Attempts" | "Format";
export interface VerifyErrors {
  amountError: AmountError;
}

type Return = {
  inProgress: boolean;
  submit: (amount: string) => void;
  errors: VerifyErrors;
  submitted: boolean;
};
export const useSubmitAmount = (baseURL: string): Return => {
  const initialErrors: VerifyErrors = {
    amountError: "None",
  };
  const [inProgress, execute, currentErrors] = useAsync(initialErrors);
  const [submitted, setSubmitted] = useState(false);

  const submiter = (amount: string) => {
    const action = submitAmount(baseURL)(amount);
    execute(action)
      .then(() => {
        queryCache.removeQueries([stateQueryPath]);
        setSubmitted(true);
      })
      .catch(() => undefined);
  };

  return { inProgress, submit: submiter, errors: currentErrors, submitted };
};
