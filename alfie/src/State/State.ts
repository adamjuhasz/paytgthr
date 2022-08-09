export interface UserInfoV6 {
  loggedIn: boolean;
  userId: string | null;
  email: string | null;
}

interface CommonState {
  baseURL: string;
  seenIntro: boolean;
  loadedSaved: boolean;
  userInfo: UserInfoV6;
  version: number;
}

export interface StateV6 extends CommonState {
  version: 6;
}

interface StateForV7 {
  askedForReview: null | string;
  appOpens: number;
}

export interface StateV7 extends CommonState, StateForV7 {
  version: 7;
}

export interface UserInfoV8 extends UserInfoV6 {
  token: string | null;
  phone?: string;
}

export interface StateV8 extends CommonState, StateForV7 {
  userInfo: UserInfoV8;
  version: 8;
}

export interface StateV9 extends CommonState, StateForV7 {
  userInfo: UserInfoV8;
  timeSlots: {
    loadedGift?: string;
    sharedPartnerInvite?: string;
    choseCardDesign?: string;
  };
  version: 9;
}

export type State = StateV9;
