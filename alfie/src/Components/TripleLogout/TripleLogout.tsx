/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import React, { useEffect } from "react";
import { StyleSheet, View } from "react-native";
import { debounce } from "lodash";
import { useDispatch, useSelector } from "react-redux";
import { useHistory, useLocation } from "../../PlatformSpecific/react-router";
import {
  connectActionSheet,
  useActionSheet,
} from "@expo/react-native-action-sheet";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import Constants from "expo-constants";
import { defaultTo } from "lodash";
import * as Updates from "expo-updates";
import console from "../../Global/Console";

import { AlfieDispatch } from "../../State/Store";
import { State } from "../../State/State";
import { logout as logoutFn } from "../../Actions/Logout";
import { setCluster } from "../../Actions/SetCluster";
import { transparentColor } from "../Styling/Colors";

const TripleLogout = (props: React.PropsWithChildren<unknown>) => {
  const { baseURL, user } = useSelector((state: State) => ({
    baseURL: state.baseURL,
    user: state.userInfo,
  }));

  const dispatch = useDispatch<AlfieDispatch>();
  const logout = () => dispatch(logoutFn());
  const switchToDev = () => dispatch(setCluster("DEV"));
  const switchToProd = () => dispatch(setCluster("PROD"));

  const location = useLocation();
  const history = useHistory();

  const { showActionSheetWithOptions } = useActionSheet();

  useEffect(() => {
    if (Constants.appOwnership === "standalone") {
      // eslint-disable-next-line @typescript-eslint/no-unsafe-call
      void Updates.checkForUpdateAsync().then((res) => {
        if (res.isAvailable === false) {
          return;
        }

        console.log(
          `update is available, most recent RID: ${JSON.stringify(
            Updates.updateId
          )}, current RID: ${JSON.stringify(Updates.updateId)}`
        );
      });
    }
  }, []);

  let tripleTouchCount = 0;
  let singleTouchCount = 0;
  let lastTripleTouch = 0;
  let lastSingleTouch = 0;
  const resetTouchNow = () => {
    tripleTouchCount = 0;
    singleTouchCount = 0;
  };

  const resetTouch = debounce(resetTouchNow, 300, {
    leading: false,
    trailing: true,
  });

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const handler = (evt: any) => {
    const isTriple =
      evt.touchHistory !== undefined &&
      evt.touchHistory.numberActiveTouches >= 3 &&
      evt.touchHistory.mostRecentTimeStamp !== lastTripleTouch;

    if (isTriple) {
      tripleTouchCount = tripleTouchCount + 1;
      lastTripleTouch = evt.touchHistory.mostRecentTimeStamp;
    }

    const isSingle =
      evt.touchHistory !== undefined &&
      evt.touchHistory.numberActiveTouches === 1 &&
      evt.touchHistory.mostRecentTimeStamp !== lastSingleTouch;

    if (isSingle) {
      singleTouchCount = singleTouchCount + 1;
      lastSingleTouch = evt.touchHistory.mostRecentTimeStamp;
    }

    if (singleTouchCount >= 10) {
      singleTouchCount = 0;
      return true;
    }

    if (tripleTouchCount >= 3) {
      tripleTouchCount = 0;
      return true;
    }
    resetTouch();
    return false;
  };

  return (
    <View
      testID="TripleLogout"
      style={styles.fullScreen}
      onStartShouldSetResponder={handler}
      onStartShouldSetResponderCapture={handler}
      onMoveShouldSetResponder={() => false}
      onMoveShouldSetResponderCapture={() => false}
      onResponderGrant={() => {
        const releaseMode = __DEV__ ? "DEV" : "Production";

        let otherCluster = ".com";
        let clusterSwitch = () => Promise.resolve();
        switch (baseURL) {
          case "https://paytgthr.com":
            otherCluster = ".dev";
            clusterSwitch = switchToDev;
            break;

          case "https://paytgthr.dev":
          default:
            otherCluster = ".com";
            clusterSwitch = switchToProd;
            break;
        }

        const releaseId = defaultTo(Updates.updateId, "?").slice(0, 13);

        const appVersion =
          Constants.appOwnership === "standalone"
            ? `${defaultTo(Constants.nativeAppVersion, "?")}-${defaultTo(
                Constants.nativeBuildVersion,
                "?"
              )}\nRID: ${releaseId}`
            : `${defaultTo(
                Constants.nativeBuildVersion,
                "?"
              )}\nRID: ${releaseId}`;

        let usingTouch: "triple-triple" | "ten-single" = "ten-single";
        if (tripleTouchCount >= 3) {
          usingTouch = "triple-triple";
        }
        void Analytics.track("TripleLogout Opened", {
          location,
          method: usingTouch,
        });
        showActionSheetWithOptions(
          {
            title: `Connected to ${baseURL}`,
            message: `App version: ${appVersion}\nRelease build: ${releaseMode}\nLocation: ${
              location.pathname
            }\nUser: ${user.email || "Unkown"}`,
            options: [
              "Close menu", // 0
              `Switch cluster to ${otherCluster}`, // 1
              "View logs", // 2
              "Logout", // 3
            ],
            cancelButtonIndex: 0,
          },
          async (index) => {
            switch (index) {
              case 0:
                // close
                return;

              case 1:
                await Analytics.track("Debug Cluster Switched", {
                  to: otherCluster,
                });
                if (!__DEV__) {
                  // In release mode disable analytics if we're doing debug stuff
                  Analytics.disable();
                }
                await clusterSwitch();
                return;

              case 2:
                await Analytics.track("Debug View Logs");
                history.push("/app/logs");
                return;

              case 3:
                await Analytics.track("User SettingsLogout Clicked", {
                  from: "TripleLogout",
                });
                await logout();
                history.push("/");
                return;
            }
          }
        );
      }}
    >
      {props.children}
    </View>
  );
};

export default connectActionSheet(TripleLogout);

const styles = StyleSheet.create({
  fullScreen: {
    backgroundColor: transparentColor,
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
});
