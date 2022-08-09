import React from "react";
import { StyleSheet, Text, View } from "react-native";
import SafeAreaView from "../../PlatformSpecific/SafeAreaView";

import { blackColor, whiteColor } from "../Styling/Colors";
import TextStyles from "../Styling/Text";

// eslint-disable-next-line @typescript-eslint/ban-types
const ErrorBoundary = (
  _props: React.PropsWithChildren<unknown>
): JSX.Element => {
  return (
    <View style={[StyleSheet.absoluteFill, styles.background]}>
      <SafeAreaView>
        <View style={[styles.frame]}>
          <Text style={[TextStyles.fontWeight600, styles.heading]}>
            Woops, an error occured.
          </Text>
          <Text style={[TextStyles.fontWeight400, styles.text]}>
            Can you close the app and try again?
          </Text>
        </View>
      </SafeAreaView>
    </View>
  );
};

export default ErrorBoundary;

const styles = StyleSheet.create({
  background: { backgroundColor: blackColor },
  frame: {
    alignItems: "center",
    height: "100%",
    justifyContent: "center",
    padding: 10,
    width: "100%",
  },
  heading: {
    color: whiteColor,
    fontSize: 40,
    textAlign: "center",
  },
  text: {
    color: whiteColor,
    fontSize: 22,
    textAlign: "center",
  },
});
