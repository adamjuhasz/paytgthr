import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

import {
  TenSeconds,
  UnauthorizedError,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";

interface DelayKYC {
  type: "delay.KYC";
}

interface ManualKYC {
  type: "manual.kyc";
}

interface FailedKYC {
  type: "failed.kyc";
}

interface ProcessingKYC {
  type: "processing.kyc";
}

interface WaitingKYC {
  type: "waiting.kyc";
  "partner-name": string;
  "partner-email": string;
}

interface MailedKYC {
  type: "mailed.kyc";
}

export type KYCState =
  | DelayKYC
  | ManualKYC
  | FailedKYC
  | ProcessingKYC
  | WaitingKYC
  | MailedKYC;

export const getKYCState = (baseURL: string) => async (): Promise<KYCState> => {
  const url = `${baseURL}/app/signup/kyc`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body: KYCState = await res.json();
      return body;
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
      throw new Error(`getKYCState got states of ${res.status}`);
  }
};
