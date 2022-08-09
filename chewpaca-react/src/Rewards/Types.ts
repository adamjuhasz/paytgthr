import { Currency } from "../Shared/Types";

export interface RewardBoost {
  boostId: string;
  boostMatch: Record<string, unknown>;
  boostRewardInBips: number;
  boostExpiresInHr: number;
  boostName: string;
  boostMaximumPayout: Currency;
  boostActive: boolean;
  boostCreated: string;
  boostUpdated: string;
  boostUses: null | number;
}

export interface RewardBoostActivation {
  activationId: string;
  activatedReward: string;
  activatedGroup: string;
  activationCreatedAt: string;
  activationUpdatedAt: string;
  activationChangeBy: string;
  activationExpires: string;
  activationState:
    | "BoostActive"
    | "BoostCancelled"
    | "BoostExpired"
    | "BoostUsed";
  activationUsesLeft: null | number;
}
