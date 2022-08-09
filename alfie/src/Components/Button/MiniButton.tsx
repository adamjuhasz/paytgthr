import React from "react";
import { StyleSheet, Text, TouchableOpacity, ViewStyle } from "react-native";

import TextStyles from "../Styling/Text";
import { blackColor, midnightBlue, whiteColor } from "../Styling/Colors";

export interface MiniButtonProps {
  text: string;
  onPress: () => void;
  testID?: string;
}

export const MiniButton = (props: MiniButtonProps): JSX.Element => {
  return (
    <TouchableOpacity
      style={[styles.miniButton, { backgroundColor: midnightBlue }]}
      onPress={props.onPress}
      testID={props.testID}
    >
      <Text style={[TextStyles.fontWeight400, styles.miniButtonText]}>
        {props.text}
      </Text>
    </TouchableOpacity>
  );
};

export default MiniButton;

export const miniButtonsContainer: ViewStyle = {
  flexDirection: "row",
  justifyContent: "space-between",
  marginTop: 10,
};

const styles = StyleSheet.create({
  miniButton: {
    alignItems: "center",
    backgroundColor: blackColor,
    borderRadius: 20,
    height: 44,
    justifyContent: "center",
    width: "49%",
  },
  miniButtonText: {
    color: whiteColor,
    fontSize: 14,
  },
});
