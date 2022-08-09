/* global window */

import { Platform } from "react-native";

import { State } from "./State";

export const defaultBaseURL = (): string => {
  if (Platform.OS === "web" && window !== undefined) {
    const origin = window.location.origin;
    if (origin.startsWith("http://localhost:") === false) {
      return origin;
    }
  }

  return __DEV__ ? "https://paytgthr.dev" : "https://paytgthr.com";
};

export const initialState: State = {
  baseURL: defaultBaseURL(),
  seenIntro: false,
  loadedSaved: false,
  userInfo: {
    loggedIn: false,
    userId: null,
    email: null,
    token: null,
  },
  askedForReview: null,
  appOpens: 1,
  timeSlots: {},
  version: 9,
};
