/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import { ThemeContext } from "../../Theming/ThemeContext";
import TextStyles from "../../Styling/Text";
import { ReferralProgress } from "../../../Actions/Referral/Types";

interface Props {
  progress: ReferralProgress;
}

export default function RefereeProgressView(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);

  let subText = "";
  switch (props.progress.progress.tag) {
    case "PurchaseCountProgress":
      subText = `${props.progress.progress.refereeMade} / ${props.progress.progress.programRequires}`;
      break;
    case "ProgramCompleted":
      subText = "Earned!";
      break;
    case "ProgramExpired":
      subText = "Expired";
      break;
  }

  return (
    <View style={[styles.container]}>
      <View style={[styles.text]}>
        <Text
          style={[TextStyles.fontWeight400, theme.textStyle, styles.spendText]}
        >
          {props.progress.refereeName}
        </Text>
        <Text
          style={[TextStyles.fontWeight600, theme.textStyle, styles.spendText]}
        >
          {subText}
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
              width: `${props.progress.percentDone}%`,
            },
          ]}
        />
      </View>
    </View>
  );
}

const styles = StyleSheet.create({
  container: {},
  progressBarBottom: {
    borderRadius: 999,
    height: 15,
    marginTop: 5,
  },
  progrssBar: {
    backgroundColor: "#34d399",
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
