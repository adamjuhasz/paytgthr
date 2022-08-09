import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

interface GetLinkTokenResponse {
  token: string;
}

export const queryPath = "/app/change/fs/ach/plaid/linktoken";
export const getLinkToken = (
  baseURL: string
) => async (): Promise<GetLinkTokenResponse> => {
  const url = `${baseURL}${queryPath}`;

  await Analytics.track("PlaidLink GetLinkToken Attempt");

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      await Analytics.track("PlaidLink GetLinkToken");
      const body = (await res.json()) as GetLinkTokenResponse;
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw new Error(`getLinkToken had return status of ${res.status}`);
  }
};

export const mutatePath = "/app/change/fs/ach/plaid/chooseaccount";
interface ChooseAccount {
  publicToken: string;
  accountId: string;
}
export const chooseAccount = (baseURL: string) => async (
  reqbody: ChooseAccount
): Promise<void> => {
  const url = `${baseURL}${mutatePath}`;

  await Analytics.track("PlaidLink Success Attempt");

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: JSON.stringify(reqbody),
  });

  switch (res.status) {
    case 200:
      await Analytics.track("PlaidLink Success");
      await Analytics.track("FundingSource Linked");
      await Analytics.track("FundingSource Verified");
      return;

    case 403:
      await Analytics.track("PlaidLink Error AccountAlreadyLinked", {
        ...reqbody,
      });
      throw new Error(`chooseAccount had return status of ${res.status}`);

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        requestBody: reqbody,
      });
      throw new Error(`chooseAccount had return status of ${res.status}`);
  }
};
