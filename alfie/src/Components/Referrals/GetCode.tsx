/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useState } from "react";
import { Pressable, StyleSheet, Text, View } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";
import { grayBorder, grayText, pinkColor } from "../Styling/Colors";
import useTimeout from "../Hooks/UseTimeout";

interface Props {
  code: string;
  shareCode: (code: string) => void;
  copyCode: (code: string) => void;
  goBack: () => void;
}

const instructions = `1. Refer a friend (sorry, it can't be the person you partner with)
2. They make 5 purchases of $20 or more
3. You each get a $20 Tgthr Card credit!

Successfully refer more than 5 people, and we'll increase your credit to $40 for referral 6 and beyond!`;

export default function ViewReferralCode(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);
  const [buttonActive, setButtonActive] = useState(false);
  const [timeoutTime, setTimeoutTime] = useState<null | number>(null);
  const [copied, setCopied] = useState(false);

  useTimeout(() => {
    setCopied(false);
  }, timeoutTime);

  return (
    <FormScreen
      title="Refer a friend"
      navigation={{ type: "action", action: props.goBack }}
      buttons={
        <>
          <Button
            style="Primary"
            text="Share referral code"
            onPress={() => props.shareCode(props.code)}
          />
        </>
      }
    >
      <Text style={[TextStyles.fontWeight500, theme.textStyle, styles.heading]}>
        Share this code with your friends
      </Text>
      <Text
        style={[
          TextStyles.fontWeight400,
          theme.textStyle,
          styles.explainer,
          { marginTop: 10 },
        ]}
      >
        {instructions}
      </Text>
      <Pressable
        onPress={() => {
          props.copyCode(props.code);
          setCopied(true);
          setTimeoutTime(3000);
        }}
        onPressIn={() => {
          setButtonActive(true);
          setTimeoutTime(null);
        }}
        onPressOut={() => setButtonActive(false)}
        style={[styles.pressable]}
      >
        <View
          style={[
            styles.button,
            {
              borderColor: buttonActive ? pinkColor : grayBorder,
              borderWidth: buttonActive ? 2 : 1,
            },
          ]}
        >
          <Text
            style={[TextStyles.fontWeight600, theme.textStyle, styles.code]}
          >
            {props.code}
          </Text>
          <Text
            style={[
              buttonActive || copied
                ? TextStyles.fontWeight600
                : TextStyles.fontWeight400,
              theme.textStyle,
              styles.explainer,
              {
                color: buttonActive || copied ? pinkColor : grayText,
              },
            ]}
          >
            {copied ? "Copied to clipboard" : "Click here to copy"}
          </Text>
        </View>
      </Pressable>
    </FormScreen>
  );
}

const styles = StyleSheet.create({
  button: {
    alignItems: "center",
    borderRadius: 10,
    justifyContent: "center",
    minHeight: 93,
    width: "100%",
  },
  code: { fontSize: 36, textAlign: "center" },
  explainer: { fontSize: 16, paddingHorizontal: 14, textAlign: "left" },
  heading: {
    fontSize: 18,
    marginBottom: 20,
    paddingHorizontal: 14,
    textAlign: "center",
  },
  pressable: { marginTop: 95, paddingHorizontal: 18 },
});
