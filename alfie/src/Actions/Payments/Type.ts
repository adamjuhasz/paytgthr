type RoutingNum = string;
type AccountNum = string;

type CurrencyCode = string;
type Numerator = number;
type Denominator = number;

interface PaymentCompleted {
  tag: "PaymentCompleted";
}

interface PaymentPending {
  tag: "PaymentPending";
}

interface PaymentFailed {
  tag: "PaymentFailed";
  contents: { tag: string };
}

export interface PaymentFromServer {
  achinfo: null | [RoutingNum, AccountNum];
  amount: [CurrencyCode, Numerator, Denominator];
  id: string;
  method: [];
  methodid: string;
  msgsource: string;
  revision: number;
  status: PaymentCompleted | PaymentPending | PaymentFailed;
  subtype: "NormalPayment";
  text: string;
  type: "DebitFromUser" | "CreditToUser";
  user: string;
  visible: boolean;
  createdat: string;
}

export interface Payment {
  achinfo: null | [RoutingNum, AccountNum];
  amount: number;
  id: string;
  method: [];
  methodid: string;
  msgsource: string;
  revision: number;
  status: PaymentCompleted | PaymentPending | PaymentFailed;
  subtype: "NormalPayment";
  text: string;
  type: "DebitFromUser" | "CreditToUser";
  user: string;
  visible: boolean;
  createdat: Date;
}
