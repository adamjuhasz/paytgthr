import React, { useContext } from "react";
import {
  ColorValue,
  StyleSheet,
  Text,
  TextStyle,
  TouchableOpacity,
  View,
  ViewStyle,
} from "react-native";

import TextStyles from "../Styling/Text";
import { useScreenSize } from "../Styling/PhoneSizes";
export { default as LeftChevron } from "../Icons/LeftChevron";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  title: string;
  style?: ViewStyle;
  textColor?: ColorValue;
  leftIcon?: React.ReactNode;
  leftAction?: () => void;
  rightIcon?: React.ReactNode;
  rightAction?: () => void;
}

const PageHeader = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  const extraStyles = props.style === undefined ? {} : props.style;
  const chevronStyle = [AppHeaderStyle.chevronSize, AppHeaderStyle.chevron];
  const { deviceClass } = useScreenSize();

  let leftSide = <View testID="PageHeader Left Spacer" style={chevronStyle} />;
  if (props.leftAction !== undefined && props.leftIcon !== undefined) {
    leftSide = (
      <TouchableOpacity
        testID="PageHeader LeftIcon"
        style={chevronStyle}
        onPress={() => {
          if (props.leftAction !== undefined) {
            props.leftAction();
          }
        }}
      >
        {props.leftIcon}
      </TouchableOpacity>
    );
  } else if (props.leftIcon !== undefined) {
    leftSide = <View style={chevronStyle}>{props.leftIcon}</View>;
  }

  const fontStyle: TextStyle =
    deviceClass === "miniPhone" ||
    deviceClass === "phone" ||
    deviceClass === "tablet"
      ? TextStyles.nexaHeavy
      : TextStyles.nexaLight;

  const textStyles: TextStyle[] =
    props.textColor !== undefined
      ? [
          fontStyle,
          AppHeaderStyle.text,
          theme.textStyle,
          { color: props.textColor },
        ]
      : [fontStyle, AppHeaderStyle.text, theme.textStyle];

  let rightSide = (
    <View testID="PageHeader Right Spacer" style={chevronStyle} />
  );
  if (props.rightIcon !== undefined && props.rightAction !== undefined) {
    rightSide = (
      <TouchableOpacity
        testID="PageHeader RightIcon"
        style={chevronStyle}
        onPress={props.rightAction}
      >
        {props.rightIcon}
      </TouchableOpacity>
    );
  } else if (props.rightIcon !== undefined) {
    rightSide = <View style={chevronStyle}>{props.rightIcon}</View>;
  }

  return (
    <View
      style={[AppHeaderStyle.main, extraStyles]}
      testID="PageHeader Container"
    >
      {leftSide}
      <Text style={textStyles}>{props.title}</Text>
      {rightSide}
    </View>
  );
};

export default PageHeader;

export const headerHeight = 44;

export const AppHeaderStyle = StyleSheet.create({
  chevron: {
    alignItems: "center",
    justifyContent: "center",
  },
  chevronSize: {
    height: headerHeight,
    width: headerHeight,
  },
  main: {
    alignItems: "center",
    flexDirection: "row",
    height: headerHeight,
    justifyContent: "space-between",
  },
  text: {
    fontSize: 20,
  },
});
