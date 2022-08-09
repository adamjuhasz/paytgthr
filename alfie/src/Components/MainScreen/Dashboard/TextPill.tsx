import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import { ThemeContext } from "../../Theming/ThemeContext";
import TextStyles from "../../Styling/Text";
import { whiteColor } from "../../Styling/Colors";

import { IconProps } from "../../Icons/Types";

interface PillProps {
  text: string;
}

const TextPill = (props: IconProps & PillProps): JSX.Element => {
  const theme = useContext(ThemeContext);
  const backgroundColor =
    props.color === theme.textColor ? "#E3F6ED" : props.color;
  const textcolor = props.color === theme.textColor ? "#225240" : whiteColor;

  return (
    <View style={iconStyles.pillContainer}>
      <View
        style={[
          iconStyles.pill,
          {
            backgroundColor: backgroundColor,
          },
        ]}
      >
        <Text style={[TextStyles.fontWeight500, { color: textcolor }]}>
          {props.text}
        </Text>
      </View>
    </View>
  );
};

const iconStyles = StyleSheet.create({
  pill: {
    borderRadius: 40,
    paddingHorizontal: 10,
    paddingVertical: 5,
  },
  pillContainer: { alignItems: "center", flexDirection: "row" },
});

export default TextPill;
