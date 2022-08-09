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
  shareInvite: (code: string) => void;
  copyInvite: (code: string) => void;
  goBack: () => void;
}

const ViewInviteCode = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const [buttonActive, setButtonActive] = useState(false);
  const [timeoutTime, setTimeoutTime] = useState<null | number>(null);
  const [copied, setCopied] = useState(false);

  useTimeout(() => {
    setCopied(false);
  }, timeoutTime);

  return (
    <FormScreen
      title="Invite partner"
      navigation={{ type: "action", action: props.goBack }}
      buttons={
        <>
          <Button
            style="Primary"
            text="Share invite"
            onPress={() => props.shareInvite(props.code)}
          />
        </>
      }
    >
      <Text
        style={[TextStyles.fontWeight500, theme.textStyle, styles.explainer]}
      >
        Send this code to your partner so we can link you two and you can start
        using your Tgthr cards.
      </Text>
      <Text
        style={[
          TextStyles.fontWeight400,
          theme.textStyle,
          styles.explainer,
          { marginTop: 10 },
        ]}
      >
        Your Tgthr Card won't work till you partner up with someone.
      </Text>
      <Pressable
        onPress={() => {
          props.copyInvite(props.code);
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
};

export default ViewInviteCode;

const styles = StyleSheet.create({
  button: {
    alignItems: "center",
    borderRadius: 10,
    justifyContent: "center",
    minHeight: 93,
    width: "100%",
  },
  code: { fontSize: 36, textAlign: "center" },
  explainer: { fontSize: 16, paddingHorizontal: 14, textAlign: "center" },
  pressable: { marginTop: 95, paddingHorizontal: 18 },
});
