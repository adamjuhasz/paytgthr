/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { StyleSheet, Text, View } from "react-native";
import { useSelector } from "react-redux";

import { State } from "../State/State";
import TextStyles from "./Styling/Text";
import { whiteColor } from "./Styling/Colors";

const selector = (state: State) => ({
  baseURL: state.baseURL,
});

const ClusterName = (): JSX.Element => {
  const state = useSelector(selector);
  let cluster: "Production" | "Staging" = "Staging";

  switch (state.baseURL) {
    case "https://paytgthr.com":
      cluster = "Production";
      break;

    case "https://paytgthr.dev":
    default:
      cluster = "Staging";
      break;
  }

  return (
    <>
      <View style={styles.pixel} testID={`ClusterName ${cluster}`} />
      {cluster === "Staging" ? (
        <View style={styles.redBar}>
          <Text style={[TextStyles.fontWeight600, styles.clusterText]}>
            {state.baseURL.slice(-3).toUpperCase()}
          </Text>
        </View>
      ) : (
        <></>
      )}
    </>
  );
};

export default ClusterName;

const redColor = "#FF0000";

const styles = StyleSheet.create({
  clusterText: {
    color: whiteColor,
    textAlign: "center",
  },
  pixel: {
    backgroundColor: whiteColor,
    height: 1,
    left: 0,
    position: "absolute",
    top: 0,
    width: 1,
  },
  redBar: {
    backgroundColor: redColor,
    elevation: 101, // needed for zIndex on android (z-index + 1)
    height: 20,
    position: "absolute",
    right: -20,
    top: 20,
    transform: [{ rotate: "45deg" }],
    width: 100,
    zIndex: 100,
  },
});
