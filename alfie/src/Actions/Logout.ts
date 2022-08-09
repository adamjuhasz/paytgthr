import { Platform } from "react-native";
import { ThunkDispatch } from "redux-thunk";

import Alert from "../PlatformSpecific/Alert";
import Analytics from "../PlatformSpecific/SegmentAnalytcs";
import { queryCache } from "react-query";
import { State } from "../State/State";
import { Action, AlfieThunk } from "../State/Store";

export const logout = (): AlfieThunk => async (dispatch, _getState) => {
  if (Platform.OS === "web") {
    return dispatch(forceLogout());
  }

  Alert.alert("Logout?", "This will log you out immediately", [
    {
      text: "Log me out",
      style: "destructive",
      onPress: async () => {
        await dispatch(forceLogout());
      },
    },
    {
      text: "Cancel",
      style: "cancel",
      onPress: () => ({}),
    },
  ]);
};

export const forceLogout = (): AlfieThunk => async (dispatch, _getState) => {
  await logoutWithoutHistory(dispatch);
};

export const logoutWithoutHistory = async (
  dispatch: ThunkDispatch<State, void, Action>
): Promise<void> => {
  dispatch({ type: "logout" });
  await Analytics.track("Signed Out", { reason: "User Action" });
  Analytics.reset();
  queryCache.clear();
};
