import { useState } from "react";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

interface ResetPasswordInfo {
  medium: "sms" | "email";
  token: string;
  identifier: string;
  newPassword: string;
}

interface Errors {
  passwordFailure: boolean;
  mediumFailure: boolean;
  tokenFailure: null | "TokenNotFound" | "TokenExpired" | "TokenAlreadyUsed";
  message: string;
}

interface Response {
  error: Errors;
  token: string;
}

export type FetchState = "none" | "inProgress" | "submitted" | "error";

interface Return {
  state: FetchState;
  errors: Errors;
  submit: (i: ResetPasswordInfo) => Promise<void>;
}

export const useResetPassword = (baseURL: string): Return => {
  const [state, setState] = useState<FetchState>("none");
  const [errors, setErrors] = useState<Errors>({
    passwordFailure: false,
    mediumFailure: false,
    tokenFailure: null,
    message: "",
  });

  const submit = async ({
    medium,
    token,
    identifier,
    newPassword,
  }: ResetPasswordInfo) => {
    setState("inProgress");
    const formdata = new FormData();
    formdata.append("medium", medium);
    formdata.append("identifier", identifier);
    formdata.append("token", token);
    formdata.append("password", newPassword);

    const url = `${baseURL}/app/password/reset`;
    const res = await retryFetch(TenSeconds, url, {
      ...commonOptions,
      method: "POST",
      headers: commonPostHeaders,
      body: formdata,
    });

    switch (res.status) {
      case 200:
        setState("submitted");
        return;

      case 400:
      case 403: {
        setState("error");
        const body = (await res.json()) as Response;
        setErrors(body.error);
        return;
      }

      default:
        setState("error");
        await Analytics.track("HTTP Error", {
          httpStatus: res.status,
          httpUrl: url,
        });
        throw new Error(`useResetPassword had a return of ${res.status}`);
    }
  };

  return { state, submit, errors };
};
