type Currency = [string, number, number];

export const reducePayments = (accum: number, curr: Payment): number =>
  accum +
  ((curr.type === "DebitFromUser" ? 1 : -1) * curr.amount[1]) / curr.amount[2];

export interface Payment {
  achinfo: [string, string] | null;
  amount: Currency;
  createdat: string;
  fromjournal: null | string;
  id: string;
  method: { tag: string };
  methodid: string;
  revision: number;
  status:
    | { tag: "PaymentPending" }
    | { tag: "PaymentCompleted" }
    | { tag: "PaymentCancelled" }
    | { tag: "PaymentCreated" }
    | { tag: "PaymentFailed"; contents: { tag: string } };
  subtype: "NormalPayment" | "RefundVerification" | "InitialVerification";
  text: string;
  tojournal: null | string;
  type: "DebitFromUser" | "CreditToUser";
  user: string;
}
