import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

interface Inputs {
  cardId: string;
  lastFour: string;
}

export const activateCard = (baseURL: string) => async ({
  cardId,
  lastFour,
}: Inputs): Promise<void> => {
  const url = `${baseURL}/app/card/${cardId}/activate`;

  const formdata = new FormData();
  formdata.append("lastFour", lastFour);

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Card Activated", { cardId, lastFour });
      return;

    case 403:
      await Analytics.track("Card ActivationFailure", { cardId, lastFour });
      throw new Error("bad activation");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        endpoint: "/app/card/:cardid/activate",
      });
      throw new Error("unknwon error");
  }
};
