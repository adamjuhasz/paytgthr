import { Currency } from "../Shared/Types";

export type ReferralPayout =
  | { tag: "CashPayout"; contents: Currency }
  | { tag: "BoostPayout"; contents: string };

export interface ReferralProgram {
  refProgram: string;
  refWorkflow: Record<string, unknown>;
  referrerReward: [number, ReferralPayout][];
  refereeReward: ReferralPayout;
  refActive: boolean;
  refOpenAccess: boolean;
  refCreatedAt: string;
  refUpdatedAt: string;
  refRevision: number;
  refName: string;
}

export type WorkflowProgress =
  | {
      tag: "PurchaseCountProgress";
      refereeMade: number;
      programRequires: number;
    }
  | {
      tag: "ProgramCompleted";
    }
  | {
      tag: "ProgramExpired";
    };

export interface ReferralProgress {
  progressId: string;
  referalProgram: string;
  referee: string;
  referrer: null | string;
  programExpiration: null | string;
  progress: WorkflowProgress;
  progressDisplay: number;
  progressCreatedAt: string;
  progressUpdatedAt: string;
  progressRevision: number;
}

export interface ReferralCode {
  referrerCode: string;
  codeReferrerId: null | string;
  codeProgramLinked: string;
  codeCreatedAt: string;
}
