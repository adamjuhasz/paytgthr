import {
  TenSeconds,
  commonGetHeaders,
  commonOptions,
  retryFetch,
} from "../fetchRequests";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import { CardInfo, CardModel, EnhancedCardModel } from "./Types";

export const queryPath = "/app/cards";

export const getCards = (baseURL: string) => async (): Promise<
  EnhancedCardModel[]
> => {
  const url = `${baseURL}${queryPath}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as CardModel[];
      const cards: CardModel[] = body
        .filter((card) => card.cardPlatform.tag !== "AptoPayments")
        .filter((card) => card.cardStatus !== "CLOSED")
        .sort((card1, card2) => {
          if (card1.createdAt > card2.createdAt) {
            return -1;
          }
          if (card1.createdAt < card2.createdAt) {
            return 1;
          }

          // must be equal
          return 0;
        });

      const infos: CardInfo[] = await Promise.all(
        cards.map((c) => getCardInfo(baseURL)(c))
      );
      const combined: EnhancedCardModel[] = cards.map((c, index) => ({
        ...c,
        ...infos[index],
      }));

      return combined;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};

const cardQueryurl = (cardId: string) => `/app/card/${cardId}`;

const globalCardDB: Record<string, CardInfo> = {};

export const getCardInfo = (baseURL: string) => async (
  card: CardModel
): Promise<CardInfo> => {
  // do not grab frozen cards
  switch (card.cardStatus) {
    case "USERFROZEN":
    case "ADMINFROZEN":
    case "CLOSED":
    case "CREATED":
      return {
        pan: "5151515151515151",
        expMonth: "00",
        expShortYear: "99",
        cvv: "000",
      };

    case "ACTIVATED":
      break;
  }

  if (globalCardDB[card.cardId] !== undefined) {
    return globalCardDB[card.cardId];
  }

  const url = `${baseURL}${cardQueryurl(card.cardId)}`;
  const res = await retryFetch(TenSeconds, url, {
    ...commonOptions,
    method: "GET",
    headers: commonGetHeaders,
  });

  switch (res.status) {
    case 200: {
      const body = (await res.json()) as CardInfo;
      globalCardDB[card.cardId] = body;
      return body;
    }

    default:
      await Analytics.track("HTTP Error", {
        httpStatus: res.status,
        httpUrl: url,
      });
      throw res;
  }
};
