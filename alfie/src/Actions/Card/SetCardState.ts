import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

export const lockCard = (baseURL: string) => async (
  cardId: string
): Promise<void> => {
  const url = `${baseURL}/app/card/${cardId}/lock`;

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: "",
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Card Locked", { cardId });
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        endpoint: "/app/card/:cardid/lock",
      });
      throw new Error("unknwon error");
  }
};

export const unlockCard = (baseURL: string) => async (
  cardId: string
): Promise<void> => {
  const url = `${baseURL}/app/card/${cardId}/unlock`;

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: "",
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Card Unlocked", { cardId });
      return;

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        endpoint: "/app/card/:cardid/unlock",
      });
      throw new Error("unknwon error");
  }
};
