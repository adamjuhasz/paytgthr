import React from "react";
import {
  Platform,
  StatusBar as RNStatusBar,
  StatusBarProps,
  StyleProp,
  StyleSheet,
  View,
  ViewStyle,
} from "react-native";
import {
  blackColor,
  greenColor,
  pinkColor,
  purpleColor,
  whiteColor,
} from "../Styling/Colors";
export { blackColor, greenColor, pinkColor, purpleColor };

export const StatusBar = ({
  backgroundColor,
  ...props
}: StatusBarProps): JSX.Element => {
  let background =
    backgroundColor === undefined ? "transparent" : backgroundColor;
  if (Platform.OS === "ios") {
    background = "transparent";
  }
  return <RNStatusBar backgroundColor={background} {...props} />;
};

export interface BackgroundProps {
  style?: StyleProp<ViewStyle>;
}

export const WhiteBackground = (props: BackgroundProps): JSX.Element => (
  <View
    style={[styles.fullscreen, { backgroundColor: whiteColor }, props.style]}
  >
    <StatusBar barStyle="dark-content" backgroundColor={whiteColor} />
  </View>
);

export const BlackBackground = (props: BackgroundProps): JSX.Element => (
  <View
    style={[styles.fullscreen, { backgroundColor: blackColor }, props.style]}
  >
    <StatusBar barStyle="light-content" backgroundColor={blackColor} />
  </View>
);

export const styles = StyleSheet.create({
  fullscreen: {
    height: "100%",
    width: "100%",
  },
});
