type CurrencyCode = string;
type Numerator = number;
type Denominator = number;

export interface LedgerFromServer {
  balance: [CurrencyCode, Numerator, Denominator];
  revision: number;
  updated: string;
}

export interface Ledger {
  balance: number;
  revision: number;
  updated: Date;
}
