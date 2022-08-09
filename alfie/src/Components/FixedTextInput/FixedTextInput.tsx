/* eslint-disable react-native/no-inline-styles */
/* eslint-disable react-native/no-color-literals */
import React, { useContext, useEffect, useState } from "react";
import {
  ColorValue,
  StyleProp,
  StyleSheet,
  Text,
  TextInput,
  TextStyle,
  View,
} from "react-native";
import { clamp, defaultTo } from "lodash";

import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";
import { pinkColor } from "../Styling/Colors";

interface Props {
  placeholder: string;
  onChangeText?: (t: string) => void;
  textInputStyle?: StyleProp<TextStyle>;
  textStyle?: StyleProp<TextStyle>;
  placeholderTextColor?: ColorValue;
  selectionColor?: ColorValue;
}

const stringToArray = (str: string): string[] =>
  str === ""
    ? [" "]
    : Array(str.length)
        .fill(" ")
        .map((_, index) => str.charAt(index));

const wideTextInput = 8;

export default function FixedTextInput(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);
  const [text, setText] = useState<string[]>(
    Array(props.placeholder.length).fill(" ")
  );
  const refs: React.RefObject<TextInput>[] = text.map(() => React.createRef());

  useEffect(() => {
    if (props.onChangeText !== undefined) {
      props.onChangeText(text.join("").replace(/\W/g, ""));
    }
  }, [props, text]);

  return (
    <View style={[styles.container]}>
      {text.map(
        (val, index): JSX.Element => {
          const isAlphaNum = /\w/;
          const isNumber = /\d/;
          const placeHolder = props.placeholder.charAt(index);
          const isSpot = isAlphaNum.test(placeHolder);

          if (isSpot) {
            return (
              <TextInput
                key={`${index}${placeHolder}`}
                placeholder={props.placeholder[index]}
                value={val === " " ? undefined : val}
                placeholderTextColor={defaultTo(
                  props.placeholderTextColor,
                  theme.scheme === "light" ? "#ABABAB" : "#676767"
                )}
                selectionColor={defaultTo(props.selectionColor, pinkColor)}
                selectTextOnFocus
                style={[
                  TextStyles.fontWeight400,
                  styles.text,
                  styles.textInput,
                  props.placeholder.length > wideTextInput
                    ? styles.smallTextInput
                    : {},
                  {
                    color: theme.textColor,
                    borderColor:
                      theme.scheme === "light" ? "#EBEBEB" : "#3E3E3E",
                  },
                  props.textInputStyle,
                ]}
                keyboardType={
                  isNumber.test(placeHolder) ? "number-pad" : "default"
                }
                onSubmitEditing={() => {
                  let nextBlock = clamp(index + 1, text.length - 1);
                  nextBlock = isAlphaNum.test(
                    props.placeholder.charAt(nextBlock)
                  )
                    ? nextBlock
                    : clamp(nextBlock + 1, 0, text.length - 1);

                  refs[nextBlock].current?.focus();
                }}
                returnKeyType="next"
                onChangeText={(t) => {
                  const newText = text.slice(0);
                  const arrayed = stringToArray(t);

                  newText.splice(index, arrayed.length, ...arrayed);
                  setText(newText);

                  const toMove = t === "" ? -1 : t.length;
                  let nextBlock = clamp(index + toMove, text.length - 1);
                  nextBlock = isAlphaNum.test(
                    props.placeholder.charAt(nextBlock)
                  )
                    ? nextBlock
                    : clamp(
                        nextBlock + (toMove > 0 ? 1 : -1),
                        0,
                        text.length - 1
                      );

                  refs[nextBlock].current?.focus();
                }}
                ref={refs[index]}
              />
            );
          } else {
            return (
              <Text
                key={index}
                style={[
                  TextStyles.fontWeight600,
                  styles.text,
                  props.placeholder.length > wideTextInput
                    ? styles.smallText
                    : {},
                  { color: theme.textColor },
                  props.textStyle,
                ]}
              >
                {props.placeholder[index]}
              </Text>
            );
          }
        }
      )}
    </View>
  );
}

const styles = StyleSheet.create({
  container: {
    alignItems: "center",
    flexDirection: "row",
    justifyContent: "center",
  },
  smallText: {
    marginHorizontal: 0,
  },
  smallTextInput: {
    marginHorizontal: 1,
    width: 35,
  },
  text: { fontSize: 18, marginHorizontal: 4 },
  textInput: {
    borderRadius: 15,
    borderWidth: 1,
    height: 50,
    textAlign: "center",
    textAlignVertical: "center",
    width: 42,
  },
});
