import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

export const changePhone = (baseURL: string) => async (
  phone: string
): Promise<void> => {
  const url = `${baseURL}/app/signup/phone/change`;

  const formdata = new FormData();
  formdata.append("phone", phone);

  await Analytics.track("User PhoneChange Attempt", { phone });

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Phone Entered", { phone });
      await Analytics.setTrait("phone", `+1${phone}`);
      return;

    case 400:
      await Analytics.track("Phone Entry Error", { reason: "format", phone });
      throw "Format";

    case 409:
      await Analytics.track("Phone Entry Error", {
        reason: "Bad user state",
        phone,
      });
      throw new Error("Bad user state");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { phone },
      });
      throw new Error("unknwon error");
  }
};

export const verifyPhone = (baseURL: string) => async (
  code: string
): Promise<void> => {
  const url = `${baseURL}/app/signup/phone/verify`;

  await Analytics.track("User PhoneVerify Attempt", { code });

  const formdata = new FormData();
  formdata.append("code", code);

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("User PhoneVerified", { code });
      return;

    case 403:
      await Analytics.track("User PhoneVerify Error", {
        codeSent: code,
        error: "Bad code",
      });
      throw new Error("Bad code");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: {
          code,
        },
      });
      throw new Error("unknwon error");
  }
};
