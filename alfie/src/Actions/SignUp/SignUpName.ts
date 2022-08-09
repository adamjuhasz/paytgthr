import { useState } from "react";
import { queryCache } from "react-query";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import { formdataHeaders } from "../../Helpers/FormData";

import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { useAsync } from "../cancellablePromise";
import { UserModel } from "../GetUserType";

export const getName = (baseURL: string) => async (): Promise<UserModel> => {
  const url = `${baseURL}/app/signup/name`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body: UserModel = await res.json();
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getName had return status of ${res.status}`);
  }
};

export const submitName = (baseURL: string) => async (
  firstName: string,
  lastName: string
): Promise<void> => {
  const formdata = new FormData();
  formdata.append("firstname", firstName);
  formdata.append("lastname", lastName);

  await Analytics.track("User NameChange Attempt", { firstName, lastName });

  const url = `${baseURL}/app/signup/name`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: {
      ...commonPostHeaders,
      ...formdataHeaders(formdata),
    },
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Name Entered", { firstName, lastName });
      await Analytics.setTrait("firstName", firstName);
      await Analytics.setTrait("lastName", lastName);
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { firstName, lastName },
      });
      throw new Error(`submitName had return status of ${res.status}`);
  }
};

type Return = {
  inProgress: boolean;
  submit: (firstName: string, lastName: string) => void;
  errors: unknown;
  success: boolean;
};
export const useSubmitName = (baseURL: string): Return => {
  const [inProgress, execute, currentErrors] = useAsync({});
  const [success, setSuccess] = useState(false);

  const submit = async (firstName: string, lastName: string) =>
    execute(submitName(baseURL)(firstName, lastName))
      .then(() => {
        queryCache.removeQueries(["/app/signup/name"]);
        setSuccess(true);
      })
      .catch(() => undefined);

  return { inProgress, submit, errors: currentErrors, success };
};
