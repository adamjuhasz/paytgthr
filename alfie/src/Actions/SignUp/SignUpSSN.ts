import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

interface SSNErrors {
  "ssn-error": boolean;
  "ssn-reuse-error": boolean;
}

export const changeSSN = (baseURL: string) => async (
  ssn: string
): Promise<SSNErrors> => {
  const url = `${baseURL}/app/signup/ssn`;

  const formdata = new FormData();
  formdata.append("ssn", ssn);

  await Analytics.track("User SSNChange Attempt");

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("SSN Entered");
      await Analytics.track("PII Entered");
      return { "ssn-error": false, "ssn-reuse-error": false };

    case 400: {
      const body: SSNErrors = await res.json();
      await Analytics.track("SSN Entry Failed", {
        reason: "Bad Entry",
        ...body,
      });
      return body;
    }

    case 409:
      await Analytics.track("SSN Entry Failed", { reason: "Bad User State" });
      throw new Error("Bad user state");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: {},
      });
      throw new Error("unknwon error");
  }
};
