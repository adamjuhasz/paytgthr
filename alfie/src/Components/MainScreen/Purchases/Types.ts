export interface PurchaseProps {
  purchasedAt: string;
  purchasedById: string;
  amount: string;
  isDeclined: boolean;
  isPending: boolean;
  userRatio: string;
  partnerRatio: string;
  description: string;
  mccEmoji: string;
  userShare: string;
  testID?: string;
  id: string;
  isFake?: boolean;
}
