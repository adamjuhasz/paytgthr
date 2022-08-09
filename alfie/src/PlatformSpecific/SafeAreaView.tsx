import React from "react";
import {
  Platform,
  SafeAreaView,
  StyleProp,
  StyleSheet,
  ViewProps,
  ViewStyle,
} from "react-native";
import { useScreenSize } from "../Components/Styling/PhoneSizes";

const SafeAreaViewWeb = (
  props: React.PropsWithChildren<ViewProps>
): JSX.Element => {
  const { deviceClass } = useScreenSize();
  let styleList: StyleProp<ViewStyle> = Array.isArray(props.style)
    ? props.style
    : [props.style];
  switch (deviceClass) {
    case "miniPhone":
    case "phone":
      styleList = [...styleList, styles.safearea, styles.fullWidth];
      break;

    case "tablet":
    case "desktop":
      styleList = [...styleList, styles.safearea, styles.limitedWidth];
      break;
  }

  switch (Platform.OS) {
    case "web":
      styleList = [...styleList, styles.webMaxHeight];
      break;

    case "android":
    case "ios":
    case "macos":
    case "windows":
      break;
  }

  return (
    <SafeAreaView {...props} style={styleList}>
      {props.children}
    </SafeAreaView>
  );
};

export default SafeAreaViewWeb;
export const MaxWidth = 500;

const styles = StyleSheet.create({
  fullWidth: {
    width: "100%",
  },
  limitedWidth: {
    maxWidth: MaxWidth,
  },
  safearea: {
    height: "100%",
    marginLeft: "auto",
    marginRight: "auto",
  },
  webMaxHeight: {
    maxHeight: "100vh",
  },
});
