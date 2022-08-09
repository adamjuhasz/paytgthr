// eslint-disable eslint-comments/no-unused-disable prettier/prettier

import { useState } from "react";
import Alert from "../PlatformSpecific/Alert";
import Analytics from "../PlatformSpecific/SegmentAnalytcs";
import { retryFetch } from "./fetchRequests";
import console from "../Global/Console";

import { AlfieThunk } from "../State/Store";
import { commonOptions, commonPostHeaders } from "./fetchRequests";
import { UserState } from "../Types/UserStateTypes";
import { formdataHeaders } from "../Helpers/FormData";

interface LoginResponse {
  bademail?: boolean | null;
  passwordwrong?: boolean | null;
  userdoesntexist?: boolean | null;
  passwordrules?: boolean | null;
  infra_error?: boolean | null;
}

const loginFetch = async (url: string, email: string, password: string) => {
  const formdata = new FormData();
  formdata.append("email", email.trim());
  formdata.append("password", password.trim());

  return retryFetch(2000, url, {
    ...commonOptions,
    method: "POST",
    headers: {
      ...commonPostHeaders,
      ...formdataHeaders(formdata),
    },
    body: formdata,
  });
};

const loginTheUser = (res: Response): AlfieThunk => async (dispatch) => {
  const token = res.headers.get("token");
  if (token === null) {
    throw new Error("token header was empty");
  }
  const body = (await res.json()) as UserState;
  const userId = body.user.id;
  const email = body.user.email;

  dispatch({
    type: "login",
    userId,
    email,
    token,
  });
  await Analytics.identify(userId, {
    email,
  });
};

const generateErrors = (jsonBody: LoginResponse) => {
  const errors: LoginError = {
    emailError: "None",
    passwordError: "None",
  };
  if (jsonBody.bademail === true) {
    errors.emailError = "EmailFormat";
  }
  if (jsonBody.passwordwrong === true) {
    errors.passwordError = "PasswordWrong";
  }
  if (jsonBody.userdoesntexist === true) {
    errors.emailError = "EmailMissing";
  }
  if (jsonBody.passwordrules === true) {
    errors.passwordError = "PasswordRule";
  }
  if (jsonBody.infra_error === true) {
    alertForInfra();
  }
  return errors;
};

const alertForInfra = () => {
  Alert.alert(
    "Error logging in",
    "We experienced an error trying to login, try again in a minute."
  );
};

export const login = (email: string, password: string): AlfieThunk => async (
  dispatch,
  getState
) => {
  const url = getState().baseURL + "/app/login";
  const res = await loginFetch(url, email, password);

  console.log("login status", res.status);
  switch (res.status) {
    case 200:
      await dispatch(loginTheUser(res));
      await Analytics.track("Signed In");
      return;

    case 400:
    case 401:
    case 404: {
      const jsonBody = (await res.json()) as LoginResponse;
      const errors = generateErrors(jsonBody);

      await Analytics.track("Signed In Error", {
        httpStatus: res.status,
        httpUrl: url,
        ...errors,
      });
      throw errors;
    }

    case 500:
    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      alertForInfra();
      return;
  }
};

export const signup = (email: string, password: string): AlfieThunk => async (
  dispatch,
  getState
) => {
  const url = getState().baseURL + "/app/signup";
  const res = await loginFetch(url, email, password);

  switch (res.status) {
    case 200:
      await dispatch(loginTheUser(res));
      await Analytics.track("Signed Up");
      return;

    case 400:
    case 401:
    case 404: {
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });

      const jsonBody = (await res.json()) as LoginResponse;

      console.error(`Error with signup, response code ${res.status}`, jsonBody);

      const errors = generateErrors(jsonBody);

      await Analytics.track("Signed Up Error", {
        email,
        httpStatus: res.status,
        httpUrl: url,
        ...errors,
      });

      throw errors;
    }

    case 500:
    default: {
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });

      const textBody = await res.text();

      console.error(`Error with signup, response code ${res.status}`, textBody);
      alertForInfra();
      return;
    }
  }
};

export type EmailError = "None" | "EmailFormat" | "EmailMissing";
export type PasswordError = "None" | "PasswordRule" | "PasswordWrong";
export interface LoginError {
  emailError: EmailError;
  passwordError: PasswordError;
}

export function isLoginError(x: unknown): x is LoginError {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unsafe-member-access
  return (x as any).emailError !== undefined;
}

type LoginFunction<T> = (email: string, password: string) => Promise<T>;
type UseLoginFunction<T> = (
  fn: LoginFunction<T>,
  email: string,
  password: string
) => void;

export const useLogin = <T>(): {
  submit: UseLoginFunction<T>;
  inProgress: boolean;
  emailErrors: EmailError;
  passwordErrors: PasswordError;
  success: boolean;
} => {
  const [inProgress, setInProgress] = useState(false);
  const [emailErrors, setEmailError] = useState<EmailError>("None");
  const [passwordErrors, setPasswordError] = useState<PasswordError>("None");
  const [success, setSuccess] = useState(false);

  const doLogin = (fn: LoginFunction<T>, email: string, password: string) => {
    setInProgress(true);
    setEmailError("None");
    setPasswordError("None");

    const promise = fn(email, password);
    promise
      .then(() => {
        setSuccess(true);
      })
      .catch((e: unknown) => {
        setInProgress(false);
        if (isLoginError(e)) {
          setEmailError(e.emailError);
          setPasswordError(e.passwordError);
        }
      });
  };

  return { submit: doLogin, inProgress, emailErrors, passwordErrors, success };
};
