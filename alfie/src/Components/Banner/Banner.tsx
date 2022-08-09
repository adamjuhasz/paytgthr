/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */

import React from "react";
import { Platform, StyleSheet, Text, TextStyle, View } from "react-native";
import { LinearGradient } from "expo-linear-gradient";

import TextStyles from "../Styling/Text";
import { whiteColor } from "../Styling/Colors";

export const colors = [
  "SocialRed",
  "SocialPurple",
  "SocialBlue",
  "SocialGreen",
  "SocialOrange",
] as const;
type Color = typeof colors[number];

interface Props {
  color: Color;
  text: React.ReactNode;
  width?: number;
  textStyle?: TextStyle;
  noShadow?: boolean;
}

const color2Gradient = (color: Color): [[string, string], string] => {
  switch (color) {
    case "SocialRed":
      return [["#F65E93", "#FD7776"], "rgba(251, 111, 128, 0.7)"];
    case "SocialPurple":
      return [["#F158C9", "#C955FF"], "rgba(240, 88, 203, 0.7)"];
    case "SocialBlue":
      return [["#1FD2DB", "#5D79EC"], "rgba(47, 187, 224, 0.7)"];
    case "SocialGreen":
      return [["#3AD79E", "#36B2B9"], "rgba(58, 215, 158, 0.7)"];
    case "SocialOrange":
      return [["#FDAD34", "#FC7F4A"], "rgba(253, 163, 57, 0.7)"];
  }
};

const Banner = (props: Props): JSX.Element => {
  const colors = color2Gradient(props.color);
  let shownShadows =
    Platform.OS === "ios" ? [shadows.main, { shadowColor: colors[1] }] : [];
  if (props.noShadow === true) {
    shownShadows = [];
  }
  return (
    <View
      style={[
        styles.container,
        styles.border,
        { width: props.width === undefined ? "100%" : props.width },
        { backgroundColor: colors[0][0] },
        ...shownShadows,
      ]}
    >
      <LinearGradient
        style={[
          StyleSheet.absoluteFill,
          styles.border,
          styles.gradient,
          { backgroundColor: colors[0][0] },
        ]}
        colors={colors[0]}
      />
      <Text
        style={[TextStyles.fontWeight400, styles.baseText, props.textStyle]}
      >
        {props.text}
      </Text>
    </View>
  );
};

export default Banner;

export const height = 94;
export const fontSize = 15;

const styles = StyleSheet.create({
  baseText: {
    color: whiteColor,
    fontSize: fontSize,
    textAlign: "center",
    zIndex: 10,
  },
  border: { borderRadius: 20 },
  container: {
    alignItems: "center",
    backgroundColor: "#F65E93",
    height: height,
    justifyContent: "center",
    paddingHorizontal: 20,
  },
  gradient: { overflow: "hidden", zIndex: 1 },
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
});
/* eslint-enable */
