import { Payment } from "../../../Actions/Payments/Type";

export const ghostPayments: Payment[] = [
  {
    achinfo: ["031101279", "••••"],
    amount: 35.98,
    id: "1",
    method: [],
    methodid:
      "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
    msgsource: "00000000-0000-0000-0000-000000000000",
    revision: 2,
    status: {
      tag: "PaymentPending",
    },
    subtype: "NormalPayment",
    text: "Tgthr Card",
    type: "DebitFromUser",
    user: "00000000-0000-0000-0000-000000000000",
    visible: true,
    createdat: new Date(Date.now() - 1 * 24 * 60 * 60 * 1000),
  },
  {
    achinfo: ["031101279", "••••"],
    amount: 12.12,
    id: "2",
    method: [],
    methodid:
      "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
    msgsource: "00000000-0000-0000-0000-000000000000",
    revision: 2,
    status: {
      tag: "PaymentPending",
    },
    subtype: "NormalPayment",
    text: "Tgthr Card",
    type: "DebitFromUser",
    user: "00000000-0000-0000-0000-000000000000",
    visible: true,
    createdat: new Date(Date.now() - 2 * 24 * 60 * 60 * 1000),
  },
  {
    achinfo: ["031101279", "••••"],
    amount: 26.54,
    id: "3",
    method: [],
    methodid:
      "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
    msgsource: "00000000-0000-0000-0000-000000000000",
    revision: 2,
    status: {
      tag: "PaymentPending",
    },
    subtype: "NormalPayment",
    text: "Tgthr Card",
    type: "DebitFromUser",
    user: "00000000-0000-0000-0000-000000000000",
    visible: true,
    createdat: new Date(Date.now() - 3 * 24 * 60 * 60 * 1000),
  },
  {
    achinfo: ["031101279", "••••"],
    amount: 56.12,
    id: "4",
    method: [],
    methodid:
      "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
    msgsource: "00000000-0000-0000-0000-000000000000",
    revision: 2,
    status: {
      tag: "PaymentCompleted",
    },
    subtype: "NormalPayment",
    text: "Tgthr Card",
    type: "DebitFromUser",
    user: "00000000-0000-0000-0000-000000000000",
    visible: true,
    createdat: new Date(Date.now() - 4 * 24 * 60 * 60 * 1000),
  },
];

export const upcomingGhostPayments = [{ amount: -12.43, date: new Date() }];

export const ghostPendingPurchases = 34.87;
