import { dashboardPath } from "../MainScreenRouter/Paths";

export const SettingsPaths = {
  dashboard: dashboardPath,
  settings: "/app/settings",
  fsList: "/app/change/fundingsources",
  link: "/app/change/link",
  plaidLink: "/app/change/fs/ach/plaid",
  manualLink: "/app/change/manuallink/entry",
  manualLinkWait: "/app/change/manuallink/waiting",
  manualLinkVerify: "/app/change/manuallink/verify",
  ratio: (id: string): string => `/app/change/groups/${id}/split`,
  viewInviteCode: "/app/change/invite/code",
  acceptInvite: "/app/change/invite/accept",
  activateCardPath: (cardId: string): string => `/app/card/${cardId}/activate`,
  setCardPinPath: (cardId: string): string => `/app/card/${cardId}/pin`,
};

export default SettingsPaths;
