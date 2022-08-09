import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";
import { CardType } from "./Types";

export interface Inputs {
  type: CardType;
}

export const createCard = (baseURL: string) => async ({
  type,
}: Inputs): Promise<void> => {
  const url = `${baseURL}/app/card/new/${type}`;

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: JSON.stringify({}),
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Card Created", { type });
      return;

    case 400:
      await Analytics.track("Card Created Failure", { type });
      throw new Error("bad creation");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        endpoint: "/app/card/new/:type",
      });
      throw new Error("unknwon error");
  }
};
