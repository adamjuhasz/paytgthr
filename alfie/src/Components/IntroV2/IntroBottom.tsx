import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  title: string | React.ReactNode;
  body: string | React.ReactNode;
  buttons: React.ReactNode;
}

const IntroBottom = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <View style={[styles.container]}>
      <View style={[styles.topText]}>
        {typeof props.title === "string" ? (
          <Text
            style={[
              TextStyles.nexaHeavy,
              TextStyles.centered,
              styles.title,
              theme.textStyle,
            ]}
          >
            {props.title}
          </Text>
        ) : (
          props.title
        )}
        {typeof props.body === "string" ? (
          <Text
            style={[
              TextStyles.fontWeight400,
              TextStyles.centered,
              theme.textStyle,
              styles.body,
            ]}
          >
            {props.body}
          </Text>
        ) : (
          props.body
        )}
      </View>
      <View style={[styles.bottomButtons]}>{props.buttons}</View>
    </View>
  );
};

export default IntroBottom;

export const TitleFontSize = 30;
export const BodyFontSize = 16;

export const styles = StyleSheet.create({
  body: {
    fontSize: BodyFontSize,
    marginTop: 16,
  },
  bottomButtons: {
    paddingHorizontal: 28,
    width: "100%",
  },
  container: {
    flexDirection: "column",
    flexGrow: 1,
    marginTop: 10,
    minHeight: 1,
    width: "100%",
  },
  title: {
    fontSize: TitleFontSize,
  },
  topText: {
    alignItems: "center",
    flexDirection: "column",
    flexGrow: 2,
    justifyContent: "center",
    paddingHorizontal: 28,
    width: "100%",
  },
});
