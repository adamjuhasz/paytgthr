export interface PartnerInvite {
  groupCreated: null | string;
  inviteCode: string;
  inviteCreated: string;
  inviteRevision: number;
  inviteStatus: "Created" | "Accepted" | "Cancelled";
  inviteUpdated: string;
  inviteePreFilled: Record<string, null>;
  inviter: string;
}
