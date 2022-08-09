import React, { useContext } from "react";
import { StyleSheet, Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import ContactSupport from "../Button/ContactSupport";
import { useStrings } from "../../Strings/Strings";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  gotoSMSCode: () => void;
  gotoEmailCode: () => void;
  goBack: () => void;
}

const PasswordForgot = (props: Props): JSX.Element => {
  const { t } = useStrings();
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      navigation={{ type: "action", action: props.goBack }}
      title={t.PasswordForgot.title}
      buttons={
        <>
          <Button
            style="Primary"
            text={t.PasswordForgot.buttons.resetSMS}
            onPress={props.gotoSMSCode}
          />
          <Button
            style="Primary"
            text={t.PasswordForgot.buttons.resetEmail}
            onPress={props.gotoEmailCode}
          />
          <ContactSupport
            style="Secondary"
            emailSubject="I need to reset my password"
          />
        </>
      }
    >
      <Text
        style={[
          TextStyles.fontWeight400,
          theme.textStyle,
          styles.explainerText,
        ]}
      >
        {t.PasswordForgot.explainer}
      </Text>
    </FormScreen>
  );
};

export default PasswordForgot;

const styles = StyleSheet.create({
  explainerText: { fontSize: 24, marginBottom: 20, paddingHorizontal: 10 },
});
