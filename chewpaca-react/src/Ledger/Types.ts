type Currency = [string, number, number];

export interface Journal {
  journalBalance: Currency;
  journalCreated: string;
  journalId: string;
  journalName: string;
  journalPendingBalance: Currency;
  journalRevision: number;
  journalTransaction: string;
  journalType:
    | { tag: "PayTgthr"; contents: string }
    | { tag: "PayTgthrRewards"; contents: string }
    | { tag: "StashTgthr"; contents: string }
    | { tag: "SaveTgthr"; contents: string }
    | { tag: "SecurtyDeposit"; contents: string }
    | {
        tag: "FundingSource";
        contents: [string, { tag: "DwollaACH"; contents: string }];
      }
    | { tag: "ExternalAccount" }
    | { tag: "VirtualAccount" };
  journalUpdated: string;
  journalUser: null | string;
  lastJournalEntry: string;
}

export type LedgerFact =
  | { kind: "trxadjustment"; body: [string, Currency] }
  | { kind: "paymentcleared"; body: [string, Currency] }
  | { kind: "initialbalance"; body: Currency }
  | { kind: "manual"; body: Currency }
  | { kind: "usertransfer"; body: Currency };

export interface LedgerEntry {
  balance: Currency;
  createdat: string;
  fact: LedgerFact;
  id: string;
  idempotency: string;
  journal: string;
  pendingbalance: Currency;
  revision: number;
  transaction: string;
  user: string;
  version: string;
}

export interface LedgerTransaction {
  ltxId: string;
  ltxCreated: string;
  ltxFact: LedgerFact;
  ltxEntries:
    | { tag: "Entries"; contents: LedgerEntry[] }
    | { tag: "EntriesById"; contents: string[] };
  ltxIdempotency: string;
  ltxUpdated: string;
}
