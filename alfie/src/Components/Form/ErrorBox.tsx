import React, { useContext } from "react";
import { StyleSheet, Text, TouchableOpacity, View } from "react-native";

import { marginSizes } from "./Styling";
import TextStyles from "../Styling/Text";
import ShieldIcon from "../Icons/Shield";
import ExclamationPointIcon from "../Icons/ExclamationPoint";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  text: string;
  icon: "Security" | "Warning";
  testID?: string;
  onPress?: () => void;
}

const ErrorBox = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  let ErrorIcon = ShieldIcon;
  switch (props.icon) {
    case "Security":
      ErrorIcon = ShieldIcon;
      break;

    case "Warning":
      ErrorIcon = ExclamationPointIcon;
      break;
  }

  return (
    <TouchableOpacity
      disabled={props.onPress === undefined}
      style={[styles.frame, { backgroundColor: `${theme.textColor}14` }]}
      testID={props.testID}
      onPress={props.onPress}
    >
      <View style={[styles.iconGrandfather]}>
        <View
          style={[
            styles.iconFrame,
            { backgroundColor: `${theme.backgroundColor}` },
          ]}
        >
          <ErrorIcon color="#EA4352" size={16} />
        </View>
      </View>
      <View style={styles.textParent}>
        <Text style={[TextStyles.fontWeight400, styles.text, theme.textStyle]}>
          {props.text}
        </Text>
      </View>
    </TouchableOpacity>
  );
};

export default ErrorBox;

const padding = 14;
const iconSize = 27;

const styles = StyleSheet.create({
  frame: {
    alignItems: "center",
    borderRadius: 13,
    flexDirection: "row",
    marginBottom: marginSizes.bottom,
    marginTop: (marginSizes.bottom - marginSizes.label) * -1,
    minHeight: padding + iconSize + padding,
    padding: padding,
  },
  iconFrame: {
    alignItems: "center",
    borderRadius: 27,
    height: iconSize,
    justifyContent: "center",
    width: iconSize,
  },
  iconGrandfather: {
    alignItems: "center",
    height: "100%",
    justifyContent: "center",
    left: padding,
    position: "absolute", // @hack for Text on Android not wrapping correctly
  },
  text: {
    fontSize: 14,
  },
  textParent: {
    flexGrow: 1,
    flexShrink: 1,
    marginLeft: 27 + 10,
  },
});
