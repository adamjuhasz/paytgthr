import React, { useContext } from "react";
import { StyleSheet, Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import Label from "../Form/Label";
import TextInput from "../Form/TextInput";
import ErrorBox from "../Form/ErrorBox";
import { useStrings } from "../../Strings/Strings";
import { useTextInput } from "../Form/hooks";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

export type Mediums = "sms" | "email";

interface Props {
  submit: (code: string, password: string) => void;
  inProgress: boolean;
  passwordError: boolean;
  tokenFailure: boolean;
  tokenExpired: boolean;
  goBack: () => void;
}

const PasswordReset = (props: Props): JSX.Element => {
  const [otp, setOTP, refOTP, otpFocus] = useTextInput();
  const [password, setPassword, refPassword, passFocus] = useTextInput();
  const { t } = useStrings();
  const theme = useContext(ThemeContext);

  const changePassword = () => {
    props.submit(otp, password);
  };

  return (
    <FormScreen
      navigation={{ type: "action", action: props.goBack }}
      title={t.PasswordReset.title}
      buttons={
        <>
          <Button
            style="Primary"
            text={t.PasswordReset.buttons.resetButton}
            onPress={changePassword}
            inProgress={props.inProgress}
            disabled={props.inProgress}
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
        {t.PasswordReset.explainer}
      </Text>
      <Label>{t.PasswordReset.labels.verificationCode}</Label>
      <TextInput
        testID="PasswordReset Code"
        autoCompleteType="off"
        keyboardType="number-pad"
        textContentType="oneTimeCode"
        autoCapitalize={"none"}
        onChangeText={(text) => {
          setOTP(text);
        }}
        onSubmitEditing={passFocus}
        ref={refOTP}
        value={otp}
      />
      {props.tokenFailure === true ? (
        <ErrorBox
          text="Can you double check the code we sent?"
          icon="Security"
        />
      ) : undefined}
      {props.tokenExpired === true ? (
        <ErrorBox text="This code is too old" icon="Security" />
      ) : undefined}

      <Label>{t.PasswordReset.labels.newPassword}</Label>
      <TextInput
        testID="PasswordReset Password"
        autoCompleteType="password"
        textContentType="newPassword"
        autoCapitalize="none"
        secureTextEntry={true}
        onChangeText={(text) => {
          setPassword(text);
        }}
        onSubmitEditing={otpFocus}
        ref={refPassword}
        value={password}
      />
      {props.passwordError === true ? (
        <ErrorBox
          text="We require 8 character long passwords"
          icon="Security"
        />
      ) : undefined}
    </FormScreen>
  );
};

export default PasswordReset;

const styles = StyleSheet.create({
  explainerText: { fontSize: 24, marginBottom: 20, paddingHorizontal: 10 },
});
