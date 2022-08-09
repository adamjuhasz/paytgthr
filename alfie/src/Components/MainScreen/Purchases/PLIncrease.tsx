/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import { ThemeContext } from "../../Theming/ThemeContext";
import TextStyles from "../../Styling/Text";

interface Props {
  currentProgress: number;
  dollarsToNextLevel: number;
}

export default function PLIncrease(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);

  let text = `$${props.dollarsToNextLevel.toFixed(2).replace(".00", "")}`;
  if (props.dollarsToNextLevel === 0 || props.currentProgress === 1.0) {
    text = "Max Level";
  }

  let currentProgress = props.currentProgress;
  if (props.currentProgress === -1) {
    // loading
    text = "•••";
    currentProgress = 0;
  }

  return (
    <View style={[styles.container]}>
      <View style={[styles.text]}>
        <Text
          style={[TextStyles.fontWeight400, theme.textStyle, styles.limitText]}
        >
          Spend till limit increase
        </Text>
        <Text
          style={[TextStyles.fontWeight600, theme.textStyle, styles.spendText]}
        >
          {text}
        </Text>
      </View>
      <View
        style={[
          styles.progressBarBottom,
          { backgroundColor: theme.scheme === "light" ? "#EBEBEB" : "#3E3E3E" },
        ]}
      >
        <View
          style={[
            styles.progrssBar,
            {
              width: `${currentProgress * 100}%`,
            },
          ]}
        />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {},
  limitText: { fontSize: 13, opacity: 0.5 },
  progressBarBottom: {
    borderRadius: 999,
    height: 15,
    marginTop: 5,
  },
  progrssBar: {
    backgroundColor: "#FF9330",
    borderRadius: 999,
    height: "100%",
    maxWidth: "100%",
    minWidth: 15,
  },
  spendText: { fontSize: 16 },
  text: {
    alignItems: "center",
    flexDirection: "row",
    justifyContent: "space-between",
  },
});
