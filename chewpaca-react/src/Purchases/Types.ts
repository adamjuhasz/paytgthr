export type Currency = [string, number, number];

export interface Purchase {
  adjustments: {
    fstransactionid: null | string;
    createdat: string;
    amountlocal: Currency;
    amountbilling: Currency;
    id: string;
    type: "authorization" | "capture" | string;
  }[];
  billingamounts: [Currency, string][];
  description: string;
  details: {
    context: string;
    description: string;
    iscardpresent: boolean;
    isemv: boolean;
    isinternational: boolean;
    isonline: boolean;
    network: string;
    type: null;
  };
  displayamount: Currency;
  groupid: null | [string, number];
  id: string;
  merchant: {
    country: string;
    locality: string;
    mcc: string;
    mccdesc: string;
    name: string;
    region: string;
  };
  purchasedat: string;
  revision: number;
  rewardid: null | string;
  source: "PayWithPrivacy";
  sourceevent: string;
  sourceid: string;
  sourceidempotency: string;
  splitamounts: [string, { numerator: number; denominator: number }][];
  state:
    | { kind: "trxpending" }
    | { kind: "trxcompleted" }
    | {
        kind: "trxdeclined";
        body:
          | { kind: "paymentunlinked"; body: string[] }
          | { kind: string; body: undefined };
      }
    | { kind: "trxauthorized" };
  userid: string;
  version: string;
}
