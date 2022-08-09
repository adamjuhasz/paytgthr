/* eslint-disable react-native/no-color-literals */
import React, { useContext } from "react";
import { Platform, StyleSheet, View } from "react-native";
import { LinearGradient } from "expo-linear-gradient";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  gradient: [string, string];
  pageIndex: number;
  pageCount: number;
  background?: React.ReactNode;
  foreground?: React.ReactNode;
}

const IntroContainer = (props: React.PropsWithChildren<Props>): JSX.Element => {
  const insets = useSafeAreaInsets();
  const theme = useContext(ThemeContext);

  const bottomView =
    Platform.OS === "android" ? (
      <></>
    ) : (
      <View
        style={[
          shadows.main,
          shadows.container,
          {
            backgroundColor: props.gradient[1],
            shadowColor:
              theme.scheme === "light"
                ? `${props.gradient[1]}BB`
                : `${props.gradient[1]}99`,
          },
        ]}
      />
    );

  const topViewStyles = Platform.OS === "android" ? [shadows.main] : [];

  return (
    <View style={[StyleSheet.absoluteFill, styles.container]}>
      <theme.background style={[StyleSheet.absoluteFill]} />
      {bottomView}
      <View style={[styles.topSection, ...topViewStyles]}>
        <LinearGradient
          style={[
            StyleSheet.absoluteFill,
            { backgroundColor: props.gradient[0] },
          ]}
          colors={props.gradient}
        >
          {props.background}
          {props.foreground}
          <View style={[styles.pageBox]}>
            {Array(props.pageCount)
              .fill(null)
              .map((_v, index) => (
                <View
                  key={index}
                  style={[
                    styles.pageDot,
                    index === props.pageIndex ? styles.whitePageDot : undefined,
                  ]}
                />
              ))}
          </View>
        </LinearGradient>
      </View>
      <View style={[styles.bottomSection, { marginBottom: insets.bottom }]}>
        {props.children}
      </View>
    </View>
  );
};

export default IntroContainer;

const constants = {
  height: 388,
  borderRadius: 20,
};

const styles = StyleSheet.create({
  bottomSection: {
    flexGrow: 1,
    position: "relative",
    width: "100%",
  },
  container: {
    flexDirection: "column",
  },
  pageBox: {
    bottom: 24,
    flexDirection: "row",
    height: 8,
    justifyContent: "center",
    position: "absolute",
    width: "100%",
  },
  pageDot: {
    backgroundColor: "rgba(255,255,255,0.5)",
    borderRadius: 8,
    height: 8,
    justifyContent: "space-between",
    marginHorizontal: 4,
    width: 8,
  },
  topSection: {
    borderBottomLeftRadius: constants.borderRadius,
    borderBottomRightRadius: constants.borderRadius,
    height: constants.height,
    overflow: "hidden",
    width: "100%",
  },
  whitePageDot: {
    backgroundColor: "rgba(255,255,255,1.0)",
  },
});

/* eslint-disable react-native/no-color-literals, react-native/sort-styles */
const shadows = StyleSheet.create({
  main: {
    // https://ethercreative.github.io/react-native-shadow-generator/
    shadowColor: "rgba(255, 0, 0, 0.7)",
    shadowOffset: {
      width: 0,
      height: 12,
    },
    shadowOpacity: 0.58,
    shadowRadius: 16.0,

    elevation: 24,
  },
  container: {
    position: "absolute",
    top: 0,
    left: 0,
    width: "100%",
    height: constants.height,

    borderBottomLeftRadius: constants.borderRadius,
    borderBottomRightRadius: constants.borderRadius,
  },
});
/* eslint-enable */
