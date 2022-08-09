import { State, StateV6, StateV7, StateV8, StateV9 } from "./State";
import { initialState } from "./InitialState";

export const upgradeState = (
  saved: StateV6 | StateV7 | StateV8 | StateV9
): State => {
  switch (saved.version) {
    case 6:
      return {
        ...saved,
        userInfo: { loggedIn: false, userId: null, email: null, token: null },
        askedForReview: null,
        timeSlots: {},
        appOpens: 1,
        version: 9,
      };

    case 7:
      return {
        ...saved,
        userInfo: { loggedIn: false, userId: null, email: null, token: null },
        timeSlots: {},
        version: 9,
      };

    case 8:
      return { ...saved, timeSlots: {}, version: 9 };

    case 9:
      return { ...saved, version: 9 };

    default:
      return initialState;
  }
};
