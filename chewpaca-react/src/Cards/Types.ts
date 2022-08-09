export type DateString = string;

export type CardStatus =
  | "CLOSED"
  | "ACTIVATED"
  | "CREATED"
  | "USERFROZEN"
  | "ADMINFROZEN";

export interface DebitCard {
  activatedAt: DateString | null;
  cardDesign: "PhysicalBlack" | "virtual" | "yellow" | "pink";
  cardId: string;
  cardLastFour: string;
  cardMemo: string;
  cardPlatform:
    | { tag: "PayWithPrivacy"; contents: string }
    | { tag: "AptoPayments"; contents: string };
  cardRevision: number;
  cardStatus: CardStatus;
  cardholder: string;
  closedAt: DateString | null;
  createdAt: DateString;
  updatedAt: DateString;
}
