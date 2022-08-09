/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useState } from "react";
import { StyleSheet, Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import { ThemeContext } from "../Theming/ThemeContext";
import TextStyles from "../Styling/Text";
import FixedTextInput from "../FixedTextInput/FixedTextInput";

interface Props {
  acceptInvite: (code: string) => void;
  goBack: () => void;
  inProgress: boolean;
}

const AcceptInvite = (props: Props): JSX.Element => {
  const [code, setCode] = useState("");
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      title="Invite partner"
      navigation={{ type: "action", action: props.goBack }}
      buttons={
        <>
          <Button
            style="Primary"
            text="Accept invite"
            disabled={code.length < 6}
            onPress={() => props.acceptInvite(code)}
            inProgress={props.inProgress}
          />
        </>
      }
    >
      <Text
        style={[theme.textStyle, TextStyles.fontWeight600, styles.explainer]}
      >
        Your partner should have shared an invite code with you, enter it below
      </Text>
      <FixedTextInput
        placeholder="XYZ-123"
        onChangeText={(t) => setCode(t)}
        textInputStyle={{
          ...TextStyles.fontWeight600,
          textTransform: "capitalize",
        }}
      />
    </FormScreen>
  );
};

export default AcceptInvite;

const styles = StyleSheet.create({
  explainer: { fontSize: 16, marginBottom: 40, paddingHorizontal: 10 },
});
