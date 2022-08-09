import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import {
  TenSeconds,
  commonOptions,
  commonPostHeaders,
  retryFetch,
} from "../fetchRequests";

interface Inputs {
  cardId: string;
  pinCode: string;
}

export const setCardPin = (baseURL: string) => async ({
  cardId,
  pinCode,
}: Inputs): Promise<void> => {
  const url = `${baseURL}/app/card/${cardId}/set/pin`;

  const formdata = new FormData();
  formdata.append("pin", pinCode);

  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "POST",
    headers: commonPostHeaders,
    body: formdata,
  });

  switch (res.status) {
    case 200:
      await Analytics.track("Card PinChanged", { cardId });
      return;

    case 400:
      await Analytics.track("Card PinChangedFailure", { cardId });
      throw new Error("bad format");

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
        endpoint: "/app/card/:cardid/set/pin",
      });
      throw new Error("unknwon error");
  }
};
