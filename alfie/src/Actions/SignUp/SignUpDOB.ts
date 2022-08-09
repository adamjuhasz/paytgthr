import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

export const changeDOB = (baseURL: string) => async (
  dob: string
): Promise<void> => {
  const url = `${baseURL}/app/signup/dob`;

  const formdata = new FormData();
  formdata.append("dob", dob);

  await Analytics.track("User DateOfBirth Attempt", { dob });

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("DateOfBirth Entered", { dob });
      await Analytics.setTrait("dateOfBirth", dob);
      return;

    case 400:
      await Analytics.track("DateOfBirth Entry Failed", {
        reason: "Bad dob format",
        dob,
      });
      throw new Error("Bad dob format");

    case 409:
      await Analytics.track("DateOfBirth Entry Failed", {
        reason: "Bad user state",
        dob,
      });
      throw new Error("Bad user state");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { dob },
      });
      throw new Error("unknwon error");
  }
};
