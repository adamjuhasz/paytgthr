export type CardStatus =
  | "CREATED"
  | "ACTIVATED"
  | "USERFROZEN"
  | "ADMINFROZEN"
  | "CLOSED";

export type CardType =
  | "yellow"
  | "pink"
  | "virtual"
  | "DigitalWallet"
  | "PhysicalBlack";

export interface CardModel {
  cardId: string;
  cardPlatform: {
    tag: "PayWithPrivacy" | "AptoPayments";
    contents: string;
  };
  cardRevision: number;
  cardDesign: CardType;
  cardholder: string;
  cardStatus: CardStatus;
  cardMemo: string | null;
  createdAt: string;
  activatedAt: string | null;
  closedAt: string | null;
  updatedAt: string;
  cardLastFour: string;
}

export interface CardInfo {
  pan: string;
  expMonth: string;
  expShortYear: string;
  cvv: string;
}

export interface EnhancedCardModel extends CardModel, CardInfo {}
