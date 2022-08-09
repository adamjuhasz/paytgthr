import React, { useContext } from "react";
import { StyleProp, StyleSheet, View, ViewStyle } from "react-native";
import { ThemeContext } from "./ThemeContext";

interface Props {
  style?: StyleProp<ViewStyle>;
}

export const FullScreenAutoBackground = (
  props: React.PropsWithChildren<Props>
): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <View style={[StyleSheet.absoluteFill]}>
      <theme.background style={[StyleSheet.absoluteFill]} />
      <View style={[StyleSheet.absoluteFill, props.style]}>
        {props.children}
      </View>
    </View>
  );
};

export default FullScreenAutoBackground;
