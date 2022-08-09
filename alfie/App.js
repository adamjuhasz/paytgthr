import React, { useState } from "react";
import AppLoading from "expo-app-loading";
import { useFonts } from "expo-font";
import console from "./src/Global/Console";
import {
  OpenSans_300Light,
  OpenSans_400Regular,
  OpenSans_600SemiBold,
  OpenSans_700Bold,
} from "@expo-google-fonts/open-sans";

import App from "./src/App";
import { getData, store } from "./src/State/Store";

const loadingStore = getData(store.dispatch);

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
const AppBase = () => {
  const [storePromise, setStorePrm] = useState(false);
  const [loaded, error] = useFonts({
    "Nexa-Heavy": require("./assets/fonts/NexaHeavy.ttf"),
    "Nexa-Light": require("./assets/fonts/NexaLight.ttf"),
    OpenSans_300Light,
    OpenSans_400Regular,
    OpenSans_600SemiBold,
    OpenSans_700Bold,
  });

  if (error !== null) {
    console.error("Error loading font", error);
  }

  loadingStore.finally(() => setStorePrm(true));

  if (storePromise === false || loaded === false) {
    return <AppLoading />;
  } else {
    return <App />;
  }
};

export default AppBase;
