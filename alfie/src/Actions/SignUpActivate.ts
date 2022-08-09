import { useState } from "react";
import Analytics from "../PlatformSpecific/SegmentAnalytcs";
import { queryCache } from "react-query";
import console from "../Global/Console";

import { useAsync } from "./cancellablePromise";
import {
  TenSeconds,
  UnauthorizedError,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "./fetchRequests";
import { queryPath as stateQueryPath } from "./User/GetUserState";

interface PinResponse {
  fourError?: boolean;
  pinLength?: boolean;
  pinEqual?: boolean;
}

const submitSignupPin = (baseURL: string) => async (
  lastFour: string,
  pin: string,
  pinConfirm: string
) => {
  const formdata = new FormData();
  formdata.append("last-four", lastFour);
  formdata.append("pin", pin);
  formdata.append("pin-confirm", pinConfirm);

  const url = `${baseURL}/app/signup/pin`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Pin Entered");
      return;

    case 400: {
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });

      const body: PinResponse = await res.json();

      const errors: PinChangeErrors = {
        lastFourError: "None",
        pinError: "None",
      };

      if (body.fourError === true) {
        errors.lastFourError = "Incorrect";
      }

      if (body.pinLength === true) {
        errors.pinError = "Length";
      }

      if (body.pinEqual === true) {
        errors.pinError = "DontMatch";
      }

      await Analytics.track("Pin Entered Error", {
        httpStatus: res.status,
        httpUrl: url,
        ...errors,
      });

      throw errors;
    }

    case 401:
      await Analytics.track("Signed Out", {
        reason: "Session failure",
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new UnauthorizedError();

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`submitSignupPin got status code ${res.status}`);
  }
};

export type LastFourErrors = "None" | "Incorrect";
export type PinErrors = "None" | "Length" | "DontMatch";

export interface PinChangeErrors {
  lastFourError: LastFourErrors;
  pinError: PinErrors;
}

type Return = {
  inProgress: boolean;
  submit: (lastFour: string, pin: string, pinConfirm: string) => void;
  errors: PinChangeErrors;
  submitted: boolean;
};
export const useActivateCard = (baseURL: string): Return => {
  const initialErrors: PinChangeErrors = {
    lastFourError: "None",
    pinError: "None",
  };
  const [inProgress, execute, currentErrors] = useAsync(initialErrors);
  const [submitted, setSubmitted] = useState(false);

  const submit = (lastFour: string, pin: string, pinConfirm: string) => {
    execute(submitSignupPin(baseURL)(lastFour, pin, pinConfirm))
      .then(() => {
        queryCache.removeQueries([stateQueryPath]);
        setSubmitted(true);
      })
      .catch((err) => {
        console.error("error in useActivateCard", err);
      });
  };

  return { inProgress, submit, errors: currentErrors, submitted };
};
