/* eslint-disable @typescript-eslint/no-unsafe-member-access */
import React from "react";
import { StyleSheet, View } from "react-native";
import * as _ from "lodash";
import { useDispatch, useSelector } from "react-redux";
import { useLocation } from "../../PlatformSpecific/react-router";
import {
  connectActionSheet,
  useActionSheet,
} from "@expo/react-native-action-sheet";

import { AlfieDispatch } from "../../State/Store";
import { State } from "../../State/State";
import { logout } from "../../Actions/Logout";
import { setCluster } from "../../Actions/SetCluster";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";

const TripleLogout = (props: React.PropsWithChildren<unknown>) => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const dispatch = useDispatch<AlfieDispatch>();
  const logoutUser = () => dispatch(logout());
  const switchToDev = () => dispatch(setCluster("DEV"));
  const switchToProd = () => dispatch(setCluster("PROD"));

  const location = useLocation();
  const { showActionSheetWithOptions } = useActionSheet();

  let tripleTouchCount = 0;
  let singleTouchCount = 0;
  let lastTripleTouch = 0;
  let lastSingleTouch = 0;
  const resetTouchNow = () => {
    tripleTouchCount = 0;
    singleTouchCount = 0;
  };

  const resetTouch = _.debounce(resetTouchNow, 300, {
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

        showActionSheetWithOptions(
          {
            title: "Debug menu",
            message: `Connected to ${baseURL}\nRelease build: ${releaseMode}\nLocation: ${location.pathname}`,
            options: [
              "Close", // 0
              `Switch cluster to ${otherCluster}`, // 1
              "Logout", // 2
            ],
            cancelButtonIndex: 0,
          },
          async (index) => {
            switch (index) {
              case 0:
                // close
                return;

              case 1:
                if (!__DEV__) {
                  // In release mode disable analytics if we're doing debug stuff
                  Analytics.disable();
                }
                await clusterSwitch();
                return;

              case 2:
                await logoutUser();
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
  // eslint-disable-next-line react-native/no-color-literals
  fullScreen: {
    backgroundColor: "transparent",
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
});
