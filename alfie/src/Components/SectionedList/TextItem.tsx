import React, { useContext, useState } from "react";
import {
  Pressable,
  SectionListRenderItemInfo,
  StyleProp,
  StyleSheet,
  Text,
  TextStyle,
  View,
  ViewStyle,
} from "react-native";
import { clamp } from "lodash";

import TextStyles from "../Styling/Text";
import { pinkColor } from "../Styling/Colors";
import { Section, TextData } from "./Types";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  style?: StyleProp<ViewStyle>;
  textStyle?: StyleProp<TextStyle>;
  helpTextStyle?: StyleProp<TextStyle>;
}

const SectionedListItem = (
  props: SectionListRenderItemInfo<TextData, Section> & Props
): JSX.Element => {
  const [pressedIn, setPressIn] = useState(false);
  const theme = useContext(ThemeContext);

  return (
    <Pressable
      style={[styles.container, props.style]}
      onPressIn={() => setPressIn(true)}
      onPressOut={() => setPressIn(false)}
      disabled={props.item.action === undefined}
      onPress={props.item.action === undefined ? undefined : props.item.action}
    >
      <View style={[styles.textAndIcon]}>
        <Text
          selectable={false}
          style={[
            TextStyles.fontWeight400,
            theme.textStyle,
            styles.text,
            props.textStyle,
            props.index === 0 ? styles.firstElement : undefined,
            pressedIn ? styles.textHighlighted : undefined,
          ]}
        >
          {props.item.text}
        </Text>
        {props.item.icon === undefined ? undefined : (
          <props.item.icon
            size={20}
            color={pressedIn ? pinkColor : theme.textColor}
          />
        )}
      </View>
      {props.item.helpText === undefined ? undefined : (
        <Text
          selectable={false}
          style={[
            TextStyles.fontWeight400,
            theme.textStyle,
            styles.helpText,
            props.helpTextStyle,
          ]}
        >
          {props.item.helpText}
        </Text>
      )}
      {props.item.progress === undefined ? undefined : (
        <View
          style={[
            styles.progressContainer,
            { backgroundColor: `${props.item.progress.color}40` },
          ]}
        >
          <View
            style={[
              StyleSheet.absoluteFill,
              styles.progressBar,
              {
                backgroundColor: `${props.item.progress.color}`,
                width: `${clamp(props.item.progress.value, 0, 100)}%`,
              },
            ]}
          />
        </View>
      )}
    </Pressable>
  );
};

export default SectionedListItem;

export const helpTextFontSize = 16;

const styles = StyleSheet.create({
  container: {
    marginBottom: 17,
    marginTop: 17,
    paddingHorizontal: 10,
  },
  firstElement: {
    marginTop: 0,
  },
  helpText: { fontSize: helpTextFontSize, marginTop: 10, opacity: 0.5 },
  progressBar: { borderRadius: 10, minWidth: 10 },
  progressContainer: {
    backgroundColor: `${pinkColor}40`,
    borderRadius: 10,
    height: 10,
    marginTop: 10,
    width: "100%",
  },
  text: {
    fontSize: 18,
  },
  textAndIcon: {
    alignItems: "center",
    flexDirection: "row",
    justifyContent: "space-between",
  },
  textHighlighted: {
    color: pinkColor,
  },
});
