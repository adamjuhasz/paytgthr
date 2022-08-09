/* eslint-disable no-console */
/* global console */

import {
  Dispatch,
  Middleware,
  Reducer,
  Store,
  applyMiddleware,
  createStore,
} from "redux";
import thunk, { ThunkAction, ThunkDispatch } from "redux-thunk";
import { composeWithDevTools } from "redux-devtools-extension";
import AsyncStorage from "../PlatformSpecific/redux-storage";
import * as _ from "lodash";
import { createLogger } from "redux-logger";

import { State, StateV6, StateV7, StateV8, StateV9, UserInfoV8 } from "./State";
import { initialState } from "./InitialState";
import { upgradeState } from "./UpgradeState";

export type AlfieThunk<ReturnType = Promise<void>> = ThunkAction<
  ReturnType,
  State,
  void,
  Action
>;

export type AlfieDispatch = ThunkDispatch<State, void, Action>;

interface LoginAction {
  type: "login";
  email: string;
  userId: string;
  token: string;
}

interface SetClusterAction {
  type: "setcluster";
  baseURL: string;
}

interface statefoundAction {
  type: "statefound";
  state: State;
}

interface AskedForReviewAction {
  type: "askedForReview";
  time: Date;
}

export interface SetTokenAction {
  type: "setToken";
  token: string;
}

export interface MarkSeenAction {
  type: "markseen";
  status: boolean;
}

export interface ChangeEmail {
  type: "changeEmail";
  email: string;
}

export interface ChangePhone {
  type: "changePhone";
  phone: string;
}

interface GeneralActions {
  type: "statemissing" | "logout" | "loginerror";
}

export interface SetTimeSlot {
  type: "setTimeslot";
  key: keyof State["timeSlots"];
  value: Date;
}

export type Action =
  | GeneralActions
  | LoginAction
  | SetClusterAction
  | statefoundAction
  | AskedForReviewAction
  | SetTokenAction
  | MarkSeenAction
  | ChangeEmail
  | ChangePhone
  | SetTimeSlot;

export const getData = async (dispatch: Dispatch<Action>): Promise<void> => {
  console.log("loading state from disk @", new Date());
  try {
    const value = await AsyncStorage.getItem("@store");
    if (value !== null) {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
      let newState: StateV6 | StateV7 | StateV8 | StateV9 = JSON.parse(value);
      newState = upgradeState(newState);
      newState.appOpens += 1;

      newState.loadedSaved = true;

      dispatch({ type: "statefound", state: newState });
    } else {
      dispatch({ type: "statemissing" });
    }
  } catch (e) {
    // error reading value
    console.error("getData error", e);
  }
};

const storeData = (store: Store<State>) => async () => {
  if (__DEV__) {
    console.log("Saving store @", new Date());
  }

  try {
    const state = store.getState();
    await AsyncStorage.setItem("@store", JSON.stringify(state));
  } catch (e) {
    // saving error
    console.error("store data error", e);
  }
};

type AlfieReducer<R> = (leaf: R, action: Action) => R;

const loadedSavedReducer: AlfieReducer<boolean> = (loadedSaved, action) => {
  switch (action.type) {
    case "statefound":
      return true;

    case "statemissing":
      return true;

    default:
      return loadedSaved;
  }
};

const userInfoReducer: AlfieReducer<UserInfoV8> = (current, action) => {
  switch (action.type) {
    case "login":
      return {
        loggedIn: true,
        userId: action.userId,
        email: action.email,
        token: action.token,
      };

    case "logout":
      return {
        loggedIn: false,
        userId: null,
        email: null,
        token: null,
      };

    case "loginerror":
      return {
        loggedIn: false,
        userId: null,
        email: null,
        token: null,
      };

    case "setToken":
      return {
        ...current,
        token: action.token,
      };

    case "changeEmail":
      return {
        ...current,
        email: action.email,
      };

    case "changePhone":
      return {
        ...current,
        phone: action.phone,
      };

    default:
      return current;
  }
};

const seenIntroReducer: AlfieReducer<boolean> = (seenIntro, action) => {
  switch (action.type) {
    case "markseen":
      return action.status;

    case "logout":
      return false;

    default:
      return seenIntro;
  }
};

const clusterReducer: AlfieReducer<string> = (baseURL, action) => {
  switch (action.type) {
    case "setcluster":
      return action.baseURL;

    default:
      return baseURL;
  }
};

const reviewReducer: AlfieReducer<null | string> = (askedForReview, action) => {
  switch (action.type) {
    case "askedForReview":
      return action.time.toISOString();

    default:
      return askedForReview;
  }
};

const timeSlotReducer: AlfieReducer<Record<string, string | undefined>> = (
  timeslots,
  action
) => {
  switch (action.type) {
    case "setTimeslot":
      return {
        ...timeslots,
        [action.key]: JSON.stringify(action.value.toJSON()),
      };

    case "logout":
      return {};

    default:
      return timeslots;
  }
};

const reducer: Reducer<State, Action> = (currState = initialState, action) => {
  let newState = currState;
  switch (action.type) {
    case "statefound":
      newState = { ...action.state, loadedSaved: true };
      break;

    case "statemissing":
      newState = { ...currState, loadedSaved: true };
      break;

    default:
      break;
  }

  const newStore: State = {
    ...newState,
    loadedSaved: loadedSavedReducer(newState.loadedSaved, action),
    userInfo: userInfoReducer(newState.userInfo, action),
    seenIntro: seenIntroReducer(newState.seenIntro, action),
    baseURL: clusterReducer(newState.baseURL, action),
    askedForReview: reviewReducer(newState.askedForReview, action),
    timeSlots: timeSlotReducer(newState.timeSlots, action),
  };
  return newStore;
};

const middlewares: [Middleware<Action, State>] = [thunk];
if (__DEV__) {
  middlewares.push(createLogger({ level: "log" }));
}

export const store = createStore(
  reducer,
  initialState,
  composeWithDevTools(applyMiddleware(...middlewares))
);

store.subscribe(
  _.debounce(storeData(store), 1000, {
    leading: true,
    trailing: true,
  })
);
