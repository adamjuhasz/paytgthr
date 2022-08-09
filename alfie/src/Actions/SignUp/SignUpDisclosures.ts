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

export const submitDisclosure = async (baseURL: string): Promise<void> => {
  const formdata = new FormData();
  formdata.append("blank", "true"); // formdata can't be empty on Android

  await Analytics.track("User Disclosures Attempt");

  const url = `${baseURL}/app/signup/disclosures`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Disclosures Accepted");
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        detais: {},
      });
      throw new Error(`submitDisclosure had return of ${res.status}`);
  }
};

type Return = { inProgress: boolean; submit: () => void; submitted: boolean };
export const useDisclosures = (baseURL: string): Return => {
  const [inProgress, execute] = useAsync({});
  const [submitted, setSubmitted] = useState(false);

  const submit = async () => {
    const action = submitDisclosure(baseURL);
    return execute(action)
      .then(() => {
        setSubmitted(true);
      })
      .catch((err) => {
        console.error("error in useDisclosures", err);
      });
  };

  return { inProgress, submit, submitted };
};
