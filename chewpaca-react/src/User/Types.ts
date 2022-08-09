export type DateString = string;
export type UserID = string;

export type CloseReasons =
  | "ClosedByUser"
  | "KYCFailed"
  | "FraudyUser"
  | "OverdueBalance"
  | "DuplicateUser"
  | "ForeignDeviceDuringSignup"
  | "Archived"
  | "BannedDeviceId"
  | "BannedIPAddress"
  | "BannedDeviceType";

export interface UserModel {
  userid: string;
  addresscity: string | null;
  addressstate: string | null;
  addressstreet: string | null;
  addressstreet2: string | null;
  addresszip: string | null;
  aptokycstatus:
    | null
    | { kind: "AutoVerifyFailed"; body: string[] }
    | { kind: "Passed" };
  bankaccountname: null | string;
  bankacount: null | string;
  bankname: null | string;
  bankrouting: null | string;
  banktype: null | { type: string; contents: string };
  bankverifedamounts: null | number[];
  bankverified: null | boolean;
  becameactiveon: DateString | null;
  cardactivatedon: DateString | null;
  cardcreatedon: DateString | null;
  constentok: DateString | null;
  createdon: DateString;
  dislcosureok: DateString | null;
  dob: DateString | null;
  dwollafundingid: null | string;
  dwollaid: null | string;
  email: string;
  emailverified: boolean;
  firstname: string | null;
  firstsignin: DateString | null;
  lastname: string | null;
  password: string | null;
  phone: string | null;
  phoneverified: boolean;
  privacyaccttoken: string | null;
  revision: number;
  ssn: string | null;
  userstate:
    | {
        tag:
          | "UserActive"
          | "UserKYCDelay"
          | "UserWaitingOnPII"
          | "UserCreated"
          | "UserWaitingOnKYC"
          | "UserUpdated"
          | "UserUpdatedKYCDelay";
      }
    | {
        tag: "UserClosed";
        contents: CloseReasons;
      };
  version: string;
}

export interface Purchase {
  id: string;
}

export interface KYCAssesment {
  countAddresss: number;
  countName: number;
  countPhone: number;
  countSSN: number;
  createdAt: DateString;
  failureReasons: string[];
  inputAddress: string;
  inputName: string;
  inputPhone: string;
  inputSSN: string;
  kycPassed: boolean;
  profileId: string;
  scoreAddress: number;
  scoreName: number;
  scoreOverall: number;
  scorePhone: number;
  scoreSSN: number;
  searchId: string;
  sourceAddress: [string, number][];
  sourceName: [string, number][];
  sourcePhone: [string, number][];
  sourceSSN: [string, number][];
  userId: string;
}

export interface UserCode {
  createdon: DateString;
  expiresat: DateString;
  id: string;
  revision: number;
  token: { tag: "EmailToken" | "PhoneToken"; contents: [string, string] };
  user: string;
  version: string;
}

export interface RiskScores {
  change: number;
  createdat: string;
  fact:
    | { kind: "successfulpayment"; body: string }
    | { kind: "manualriskadj"; body: number }
    | { kind: "initialrisk" }
    | { kind: "manualriskadj"; body: number }
    | { kind: "failedpayment"; body: string }
    | { kind: "changedlinkedacct" }
    | {
        kind: "riskyacctbalance";
        body: [string, number, number];
      }
    | { kind: "riskytransaction"; body: string };
  rev: number;
  trustscore: number;
  user: string;
}
