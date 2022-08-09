import { GroupStates } from "./Group";

export type CardDisabledReasons =
  | "ErrorAccessingUser"
  | "NoActiveGroup"
  | "MultiplePermGroups"
  | "UserIsNotActive"
  | "UserIsClosed"
  | "NoFSLinked"
  | "NoFSVerified"
  | "KYCNotPassed"
  | "PartnerIsNotActive"
  | "PartnerIsClosed"
  | "PartnerNoFSLinked"
  | "PartnerNoFSVerified"
  | "PartnerKYCNotPassed";

export interface ACHState {
  abaExists: boolean;
  ddaExists: boolean;
  bankName: string | null;
  accountName: string | null;
  verified: boolean | null;
  dwollaExists: boolean | null;
  dwollaFSExists: boolean | null;
}

export interface CardState {
  status:
    | "CREATED"
    | "ACTIVATED"
    | "USERFROZEN"
    | "ADMINFROZEN"
    | "CLOSED"
    | null;
  enabled: boolean;
  disabledReason: CardDisabledReasons | null;
}

type KYCFails =
  | "PhoneScoreLow"
  | "AddressScoreLow"
  | "SSNScoreLow"
  | "NameScoreLow"
  | "WatchlistNeedsReview"
  | "WatchlistRejected"
  | "DocumentationIncorrect";

interface KYCPassed {
  kind: "Passed";
}

interface KYCAutoVerifyFailed {
  kind: "AutoVerifyFailed";
  body: [KYCFails];
}

interface KYCRejected {
  kind: "Rejected";
  body: [KYCFails];
}

export type UserStates =
  | "UserCreated"
  | "UserWaitingOnPII"
  | "UserWaitingOnKYC"
  | "UserKYCDelay"
  | "UserUpdated"
  | "UserUpdatedKYCDelay"
  | "UserClosed"
  | "UserActive";

export interface UserState {
  user: {
    id: string;
    revision: number;
    email: string;
    emailVerified: boolean;
    name: {
      first: string | null;
      last: string | null;
    };
    address: {
      street: string | null;
      street2: string | null;
      city: string | null;
      state: string | null;
      zip: string | null;
    };
    ach: ACHState;
    phone: {
      number: string | null;
      verified: boolean;
    };
    dob: string | null;
    ssn: {
      exists: boolean | null;
    };
    disclosure: Date | null;
    consent: Date | null;
    kyc: {
      status: KYCPassed | KYCAutoVerifyFailed | KYCRejected;
    };
    card: CardState;
    status: {
      tag: UserStates;
    };
    timestamps: {
      created: string;
      activated: string | null;
    };
  };
  group: {
    id: string | null;
    status: GroupStates | null;
    revision: number | null;
    ratioAccepted: boolean;
    groupAccepted: boolean;
  };
  partner: null | {
    linked: boolean;
    verified: boolean;
    status: {
      tag: UserStates;
    };
    firstName: null | string;
    lastName: null | string;
  };
}
