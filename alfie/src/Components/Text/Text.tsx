import React from "react";
import { StyleSheet, Text, TextProps, useColorScheme } from "react-native";
import { blackColor, whiteColor } from "../Styling/Colors";

export const TextWithColorScheme = (
  props: React.PropsWithChildren<TextProps>
): JSX.Element => {
  let scheme = useColorScheme();
  if (scheme === null || scheme === undefined) {
    scheme = "light";
  }

  const textColor = scheme === "light" ? blackColor : whiteColor;

  return (
    <Text
      {...props}
      style={StyleSheet.flatten([{ color: textColor }, props.style])}
    />
  );
};
