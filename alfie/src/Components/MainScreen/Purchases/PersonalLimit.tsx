/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";
import { defaultTo } from "lodash";

import { ThemeContext } from "../../Theming/ThemeContext";
import TextStyles from "../../Styling/Text";
import { pinkColor } from "../../Styling/Colors";

interface Props {
  canSpend: number;
  maxSpend: number;
}

export default function PersonalLimit(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);

  let text = `$${Math.floor(props.canSpend)} / $${Math.floor(props.maxSpend)}`;
  let canSpend = props.canSpend;
  if (props.canSpend === -1) {
    //loading
    text = "•••";
    canSpend = 0;
  }

  return (
    <View style={[styles.container]}>
      <View style={[styles.text]}>
        <Text
          style={[TextStyles.fontWeight400, theme.textStyle, styles.limitText]}
        >
          Current spend left
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
              width: `${(canSpend / defaultTo(props.maxSpend, 1)) * 100}%`,
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
    marginTop: 6,
  },
  progrssBar: {
    backgroundColor: pinkColor,
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
