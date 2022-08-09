/* eslint-disable react-native/no-inline-styles */

import React, { useContext } from "react";
import {
  ActivityIndicator,
  GestureResponderEvent,
  Text,
  TextStyle,
  TouchableOpacity,
  ViewStyle,
  useColorScheme,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";

import ButtonStyle from "./Styling";
import { ThemeContext } from "../Theming/ThemeContext";
import { midnightBlue, purpleColor, whiteColor } from "../Styling/Colors";
import TextStyles from "../Styling/Text";

export type ButtonStyle = "Primary" | "Secondary";

export interface Props {
  onPress: (event: GestureResponderEvent) => void;
  inProgress?: boolean;
  disabled?: boolean;
  style: ButtonStyle;
  text: string;
  testID?: string;
  styles?: ViewStyle;
  textStyles?: TextStyle;
}

export const Button = (props: Props): JSX.Element => {
  let scheme = useColorScheme();
  if (scheme === null || scheme === undefined) {
    scheme = "light";
  }
  const theme = useContext(ThemeContext);

  const inProgress = props.inProgress !== undefined ? props.inProgress : false;
  const disabled = props.disabled !== undefined ? props.disabled : false;

  switch (props.style) {
    case "Primary":
      return (
        <TouchableOpacity
          testID={props.testID}
          disabled={inProgress || disabled}
          style={[
            ButtonStyle.generalButton,
            {
              backgroundColor:
                props.styles?.backgroundColor === undefined
                  ? midnightBlue
                  : props.styles.backgroundColor,
            },
            props.styles,
            { overflow: "hidden" },
          ]}
          onPress={props.onPress}
        >
          <LinearGradient
            colors={
              props.styles?.backgroundColor === undefined
                ? [purpleColor, midnightBlue]
                : [
                    props.styles.backgroundColor as string,
                    props.styles.backgroundColor as string,
                  ]
            }
            style={{
              width: "100%",
              height: "100%",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            {inProgress ? (
              <ActivityIndicator color={whiteColor} />
            ) : (
              <Text
                numberOfLines={1}
                style={[
                  ButtonStyle.generalText,
                  props.textStyles,
                  { opacity: disabled ? 0.3 : undefined, color: whiteColor },
                ]}
              >
                {props.text}
              </Text>
            )}
          </LinearGradient>
        </TouchableOpacity>
      );
      break;

    case "Secondary":
      return (
        <TouchableOpacity
          testID={props.testID}
          disabled={inProgress || disabled}
          style={[
            ButtonStyle.generalButton,
            ButtonStyle.secondaryButton,
            props.styles,
            { opacity: disabled ? 0.5 : undefined },
          ]}
          onPress={props.onPress}
        >
          {inProgress ? (
            <ActivityIndicator color={purpleColor} />
          ) : (
            <Text
              numberOfLines={1}
              style={[
                ButtonStyle.generalText,
                TextStyles.fontWeight400,
                theme.textStyle,
                {
                  color: theme.scheme === "light" ? midnightBlue : purpleColor,
                },
                { opacity: disabled ? 0.5 : undefined },
              ]}
            >
              {props.text}
            </Text>
          )}
        </TouchableOpacity>
      );
  }
};

export default Button;
