import React, { useEffect, useRef } from "react";
import { Animated, StyleSheet, Text, View, useColorScheme } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { blackColor, whiteColor } from "../Styling/Colors";
import TextStyles from "../Styling/Text";

interface Props {
  toastStr: string;
  toastId: string;
}

const ToastDisplay = (props: Props): JSX.Element => {
  const insets = useSafeAreaInsets();
  let scheme = useColorScheme();
  scheme = scheme === null ? "light" : scheme;
  const fadeAnim = useRef(new Animated.Value(0)).current; // Initial value for opacity: 0
  const moveAnim = useRef(new Animated.Value(0)).current; // Initial value for opacity: 0

  useEffect(() => {
    moveAnim.setValue(0);
    Animated.timing(fadeAnim, {
      toValue: 1,
      duration: 500,
      useNativeDriver: false,
    }).start();

    moveAnim.setValue(0);
    Animated.timing(moveAnim, {
      toValue: insets.top,
      duration: 500,
      useNativeDriver: false,
    }).start();
  }, [fadeAnim, moveAnim, insets.top, props.toastStr, props.toastId]);

  return (
    <Animated.View
      style={[styles.toastWindow, { top: moveAnim, opacity: fadeAnim }]}
    >
      <View
        style={[
          styles.toastBlob,
          { backgroundColor: scheme === "light" ? whiteColor : blackColor },
        ]}
      >
        <Text
          style={[
            TextStyles.fontWeight600,
            styles.toastText,
            { color: scheme === "light" ? blackColor : whiteColor },
          ]}
        >
          {props.toastStr}
        </Text>
      </View>
    </Animated.View>
  );
};

export default ToastDisplay;

const styles = StyleSheet.create({
  toastBlob: {
    alignItems: "center",
    borderRadius: 25,
    justifyContent: "center",
    paddingHorizontal: 10,
    paddingVertical: 12,
    width: "75%",
  },
  toastText: { fontSize: 18, fontWeight: "600" },
  toastWindow: {
    alignItems: "center",
    left: 0,
    opacity: 0.85,
    position: "absolute",
    right: 0,
    top: 0,
  },
});
