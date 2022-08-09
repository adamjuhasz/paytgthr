/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React, { useContext } from "react";
import { StyleSheet, View, ViewStyle } from "react-native";

import { darkGray, lightGray, pinkColor } from "../Styling/Colors";
import { ThemeContext } from "../Theming/ThemeContext";

export interface Props {
  index: number;
  count: number;
  sizePx?: number;
}

const DotProgress = ({ sizePx = 8, ...props }: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  const normCount = Math.max(1, props.count);
  const normIndex = Math.max(Math.min(props.index, normCount), 0);

  const dotsLeft = normCount - normIndex;
  const lineLength = normIndex;
  const arr = Array(dotsLeft).fill(null);

  const grayColor = theme.scheme === "light" ? lightGray : darkGray;

  const style: ViewStyle = {
    width: sizePx,
    height: sizePx,
    borderRadius: sizePx,
    marginHorizontal: sizePx / 2,
  };

  let line = (
    <View
      style={[
        styles.dot,
        style,
        {
          width: sizePx * lineLength + Math.max(lineLength - 1, 0) * sizePx,
          backgroundColor: pinkColor,
        },
      ]}
    />
  );
  if (normIndex === 0) {
    line = <></>;
  }

  return (
    <View style={[styles.container]}>
      {line}
      {arr.map((_, index) => (
        <View
          key={index}
          style={[
            styles.dot,
            style,
            {
              backgroundColor: index === 0 ? "transparent" : grayColor,
              borderColor: index === 0 ? pinkColor : "transparent",
              borderWidth: index === 0 ? 2 : 0,
            },
          ]}
        />
      ))}
    </View>
  );
};

export default DotProgress;

const styles = StyleSheet.create({
  container: {
    flexDirection: "row",
  },
  dot: {},
});
