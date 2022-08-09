/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useState } from "react";
import { StyleSheet, Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import { ThemeContext } from "../Theming/ThemeContext";
import TextStyles from "../Styling/Text";
import FixedTextInput from "../FixedTextInput/FixedTextInput";

interface Props {
  useReferralCode: (code: string) => void;
  goBack: () => void;
  inProgress: boolean;
  inError: boolean;
}

const instructions = `1. Enter your friend’s referral code below
2. Make 5 purchases of $20 or more in the next 60 days
3. Profit (get a $20 Tgthr Card credit)!`;

const AcceptInvite = (props: Props): JSX.Element => {
  const [code, setCode] = useState("");
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      title="Enter referral code"
      navigation={{ type: "action", action: props.goBack }}
      buttons={
        <>
          <Button
            style="Primary"
            text="Use code"
            disabled={code.length < 5}
            onPress={() => props.useReferralCode(code)}
            inProgress={props.inProgress}
          />
        </>
      }
    >
      <Text style={[theme.textStyle, TextStyles.fontWeight600, styles.heading]}>
        Did your friend give you their referral code?
      </Text>
      <Text
        style={[theme.textStyle, TextStyles.fontWeight400, styles.explainer]}
      >
        {instructions}
      </Text>
      <FixedTextInput
        placeholder="R12AB"
        onChangeText={(t) => setCode(t)}
        textInputStyle={{
          ...TextStyles.fontWeight600,
          textTransform: "capitalize",
          ...(props.inError ? { color: "red" } : {}),
        }}
      />
    </FormScreen>
  );
};

export default AcceptInvite;

const styles = StyleSheet.create({
  explainer: { fontSize: 16, marginBottom: 40, paddingHorizontal: 10 },
  heading: { fontSize: 16, marginBottom: 20, paddingHorizontal: 10 },
});
