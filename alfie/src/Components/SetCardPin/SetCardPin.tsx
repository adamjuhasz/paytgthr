/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useState } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import { ThemeContext } from "../Theming/ThemeContext";
import TextStyles from "../Styling/Text";
import FixedTextInput from "../FixedTextInput/FixedTextInput";

interface Props {
  setPin: (pinCode: string) => void;
  goBack: () => void;
  inProgress: boolean;
}

export default function ActivateCard(props: Props): JSX.Element {
  const [code, setCode] = useState("");
  const [confirmCode, setConformCode] = useState("");
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      title="Set PIN"
      navigation={{ type: "action", action: props.goBack }}
      buttons={
        <>
          <Button
            style="Primary"
            text="Set PIN"
            disabled={code.length < 4 || code !== confirmCode}
            onPress={() => props.setPin(code)}
            inProgress={props.inProgress}
          />
        </>
      }
    >
      <Text
        style={[theme.textStyle, TextStyles.fontWeight600, styles.explainer]}
      >
        Please choose a PIN code for in-person purchases and then confirm your
        code
      </Text>
      <FixedTextInput
        placeholder="1111"
        onChangeText={(t) => setCode(t)}
        textInputStyle={{
          ...TextStyles.fontWeight600,
          textTransform: "capitalize",
        }}
      />
      <View style={[styles.spacer]} />
      <FixedTextInput
        placeholder="1111"
        onChangeText={(t) => setConformCode(t)}
        textInputStyle={{
          ...TextStyles.fontWeight600,
          textTransform: "capitalize",
          ...(confirmCode.length === 4 && confirmCode !== code
            ? { borderWidth: 2, borderColor: "red" }
            : {}),
        }}
      />
    </FormScreen>
  );
}

const styles = StyleSheet.create({
  explainer: { fontSize: 16, marginBottom: 40, paddingHorizontal: 10 },
  spacer: { height: 20 },
});
