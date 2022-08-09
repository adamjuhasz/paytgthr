/* eslint-disable @typescript-eslint/prefer-regexp-exec */
import React, { useEffect, useRef } from "react";
import {
  AppState,
  AppStateStatus,
  Platform,
  StyleSheet,
  View,
} from "react-native";
import { Provider, useSelector } from "react-redux";
import {
  Router,
  useHistory,
  useLocation,
} from "./PlatformSpecific/react-router";
import { ActionSheetProvider } from "@expo/react-native-action-sheet";
import { ReactQueryConfig, ReactQueryConfigProvider } from "react-query";
import console from "./Global/Console";
import Analytics from "./PlatformSpecific/SegmentAnalytcs";
import { setFocusHandler } from "react-query";
import * as Facebook from "expo-facebook";
import * as Segment from "expo-analytics-segment";
import Constants from "expo-constants";

import { State } from "./State/State";
import { store } from "./State/Store";
import DeepLinking from "./Components/DeepLinking";
import TripleLogout from "./Components/TripleLogout/TripleLogout";
import PopUpProvider from "./Components/PopUp";
import { registerForPushNotificationsAsync } from "./PlatformSpecific/PushNotifcations";
import RootRouter from "./Components/Routers/RootRouter";
import ErrorBoundary from "./Components/ErrorBoundary/ErrorBoundaryHOC";
import ClusterName from "./Components/ClusterName";
import { ToastProvider } from "./Components/Toast/ToastProvider";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "./Components/Theming/ThemeContext";
import { dashboardPath } from "./Components/Routers/MainScreenRouter/Paths";

// eslint-disable-next-line no-var, @typescript-eslint/no-unused-vars
declare var global: { HermesInternal: null | unknown };

const PushNotificationHandler = () => {
  const loc = useLocation();
  useEffect(() => {
    if (loc.pathname === dashboardPath) {
      void registerForPushNotificationsAsync();
    }
  }, [loc]);

  return <></>;
};

const AnalyticsHOC = () => {
  const history = useHistory();
  const { appOpens } = useSelector((state: State) => ({
    appOpens: state.appOpens,
  }));

  useEffect(() => {
    const unReg = history.listen((location, action) => {
      console.log("moving to", location, "due to", action);
    });

    return () => {
      console.log("unregistering history listen");
      unReg();
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const appState = useRef(AppState.currentState);

  useEffect(() => {
    if (appOpens === 1) {
      void Analytics.track("Application Installed");
    }

    void Analytics.track("Application Opened", { from_background: false });

    const changeHandler = (newState: AppStateStatus): void => {
      if (
        appState.current.match(/inactive|active/) &&
        newState.match(/background/)
      ) {
        void Analytics.track("Application Backgrounded");
      }
      if (
        appState.current.match(/inactive|background/) &&
        newState.match(/active/)
      ) {
        void Analytics.track("Application Opened", { from_background: true });
      }
      appState.current = newState;
    };

    AppState.addEventListener("change", changeHandler);
    return () => {
      AppState.removeEventListener("change", changeHandler);
    };
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  return (
    <>
      <DeepLinking />
    </>
  );
};

const reactQueryConfig: ReactQueryConfig = {
  queries: {
    staleTime: 10 * 1000, // 10 sec
    retry: 0,
    cacheTime: Platform.OS === "android" ? 1 * 60 * 1000 : 5 * 60 * 1000,
  },
};

const App = (): JSX.Element => {
  const MasterFrame = (props: React.PropsWithChildren<unknown>) => {
    const style = StyleSheet.create({
      masterFrameWeb: {
        height: "100vh",
        overflow: "hidden",
        width: "100vw",
      },
    });
    if (Platform.OS === "web") {
      // eslint-disable-next-line react/prop-types
      return <View style={style.masterFrameWeb}>{props.children}</View>;
    } else {
      // eslint-disable-next-line react/prop-types
      return <>{props.children}</>;
    }
  };

  useEffect(() => {
    const setupTracking = async () => {
      console.log("Starting analytics init");
      await Analytics.setup();

      try {
        console.log("Starting facebook init");
        await Facebook.initializeAsync("0");
        console.log("FacebookInit Success");

        const res = await Facebook.requestPermissionsAsync();
        if (res.granted) {
          await Analytics.track("App Tracking Permission Granted", { ...res });
          console.log("App Tracking Permission Granted");
          await Facebook.setAdvertiserIDCollectionEnabledAsync(true);
        } else {
          await Analytics.track("App Tracking Permission Declined", { ...res });
          console.log("App Tracking Permission Declined");
        }
        await Facebook.setAdvertiserTrackingEnabledAsync(true);
        await Facebook.setAutoLogAppEventsEnabledAsync(true);
        if (Constants.appOwnership !== "expo") {
          await Segment.setEnabledAsync(true);
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (e: any) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
        if (typeof e.toString === "function") {
          const err = e as Error;
          console.log("facebook fail", err.toString());
        } else {
          console.log("facebook fail", e);
        }

        void Analytics.track("FacebookInit Error");
      }
    };

    void setupTracking();
  });

  useEffect(() => {
    setFocusHandler((handleFocus) => {
      const handleAppStateChange = (appState: AppStateStatus) => {
        if (appState === "active") {
          handleFocus();
        }
      };
      AppState.addEventListener("change", handleAppStateChange);
      return () => AppState.removeEventListener("change", handleAppStateChange);
    });
  }, []);

  return (
    <Router>
      <Provider store={store}>
        <AnalyticsHOC />
        <MasterFrame>
          <ErrorBoundary>
            <SafeAreaProvider>
              <ThemeProvider>
                <PushNotificationHandler />
                <ClusterName />
                <ActionSheetProvider>
                  <ReactQueryConfigProvider config={reactQueryConfig}>
                    <TripleLogout>
                      <ToastProvider>
                        <PopUpProvider>
                          <RootRouter />
                        </PopUpProvider>
                      </ToastProvider>
                    </TripleLogout>
                  </ReactQueryConfigProvider>
                </ActionSheetProvider>
              </ThemeProvider>
            </SafeAreaProvider>
          </ErrorBoundary>
        </MasterFrame>
      </Provider>
    </Router>
  );
};

export default App;
