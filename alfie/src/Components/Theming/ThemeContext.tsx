import React from "react";
import {
  ColorSchemeName,
  StyleSheet,
  TextStyle,
  View,
  useColorScheme,
} from "react-native";
import {
  BackgroundProps,
  BlackBackground,
  WhiteBackground,
} from "./Background";
import { blackColor, whiteColor } from "../Styling/Colors";

interface Theme {
  scheme: ColorSchemeName;
  textColor: string;
  textStyle: TextStyle;
  backgroundColor: string;
  background: (p: BackgroundProps) => JSX.Element;
}

const lightTheme: Theme = {
  scheme: "light",
  textColor: blackColor,
  textStyle: { color: blackColor },
  backgroundColor: whiteColor,
  background: WhiteBackground,
};

const darkTheme: Theme = {
  scheme: "dark",
  textColor: whiteColor,
  textStyle: { color: whiteColor },
  backgroundColor: blackColor,
  background: BlackBackground,
};

export const ThemeContext = React.createContext<Theme>(lightTheme);

export const ThemeProvider = (
  props: React.PropsWithChildren<unknown>
): JSX.Element => {
  const scheme = useColorScheme();
  let currentTheme = lightTheme;
  switch (scheme) {
    case "dark":
      currentTheme = darkTheme;
      break;

    case "light":
      currentTheme = lightTheme;
      break;

    case null:
    case undefined:
      currentTheme = lightTheme;
      break;
  }

  return (
    <ThemeContext.Provider value={currentTheme}>
      <View
        style={[
          StyleSheet.absoluteFill,
          { backgroundColor: currentTheme.backgroundColor },
        ]}
      />
      {props.children}
    </ThemeContext.Provider>
  );
};
