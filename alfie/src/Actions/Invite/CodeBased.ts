import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

export const getInviteCodePath = "/app/invite/code";

export interface CodeResponse {
  code: string;
  created: string;
}

export const getInviteCode = (
  baseURL: string
) => async (): Promise<CodeResponse> => {
  const url = `${baseURL}${getInviteCodePath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as CodeResponse;
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getCode had return status of ${res.status}`);
  }
};

export const acceptInvitePath = "/app/invite/accept";

export const acceptInvite = (baseURL: string) => async (
  code: string
): Promise<void> => {
  const url = `${baseURL}${acceptInvitePath}`;

  const formdata = new FormData();
  formdata.append("code", code.toUpperCase().trim().replace("-", ""));

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as { groupid: string };
      await Analytics.track("User InviteAccepted", { group: body.groupid });
      return;
    }

    case 404:
      await Analytics.track("User AcceptInvite Error", {
        reason: "Code not found",
        code: code,
      });
      throw new Error("Code not found");

    case 409:
      await Analytics.track("User AcceptInvite Error", {
        reason: "Code not found",
        code: code,
      });
      throw new Error("Can't invite self");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        details: { code },
      });
      throw new Error("unknwon error");
  }
};
