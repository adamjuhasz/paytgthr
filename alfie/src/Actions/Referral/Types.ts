export interface ReferralProgress {
  percentDone: number;
  expires: string;
  created: string;
  progress:
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
  refereeName: string;
  referrerFname: string;
}
