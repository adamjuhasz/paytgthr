import { UserState } from "../../src/Types/UserStateTypes";

export const userState: UserState = {
  user: {
    id: "123",
    revision: 1,
    email: "john@email.com",
    emailVerified: true,
    name: {
      first: "John",
      last: "Smith",
    },
    address: {
      street: "123 main",
      street2: null,
      city: "Irvine",
      state: "CA",
      zip: "98843",
    },
    ach: {
      abaExists: true,
      ddaExists: true,
      bankName: "Bank Tgthr",
      accountName: "John and Jane Tgthr",
      verified: true,
      dwollaExists: true,
      dwollaFSExists: true,
    },
    phone: {
      number: "2341232345",
      verified: true,
    },
    dob: "1985-08-02T00:00:00Z",
    ssn: {
      exists: true,
    },
    disclosure: new Date("2020-08-02T00:00:00Z"),
    consent: new Date("2020-08-02T00:00:00Z"),
    kyc: {
      status: {
        kind: "Passed",
      },
    },
    card: {
      status: "ACTIVATED",
      enabled: true,
      disabledReason: null,
    },
    status: {
      tag: "UserActive",
    },
    timestamps: {
      created: new Date("2020-08-02T00:00:00Z").toISOString(),
      activated: null,
    },
  },
  group: {
    id: "xyz",
    /** @todo give enum values */
    status: "groupactive",
    revision: 1,
    ratioAccepted: true,
    groupAccepted: true,
  },
  partner: null,
};
