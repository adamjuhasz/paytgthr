import React, { useCallback, useState } from "react";
import Analaytics from "../../PlatformSpecific/SegmentAnalytcs";
import { debounce } from "lodash";

import { EmailError, PasswordError } from "../../Actions/Login";

import ErrorBox from "../Form/ErrorBox";
import FormScreen from "../Form/FormScreen";
import TextInput from "../Form/TextInput";
import Label from "../Form/Label";
import { useTextInput } from "../Form/hooks";
import Button from "../Button/Button";
import ContactSupport from "../Button/ContactSupport";
import { useStrings } from "../../Strings/Strings";
import { misspelledEmail, validFormat } from "../../Helpers/Email";

interface Props {
  inProgress: boolean;
  login: (email: string, password: string) => void;
  emailError: EmailError;
  passwordError: PasswordError;
  gotoPasswordForgot: () => void;
  goBack: () => void;
}

const debounceTimeMs = 2000;

export const LoginForm = (props: Props): JSX.Element => {
  const [email, setEmail, refEmail, emailFocus] = useTextInput();
  const [password, setPassword, refPassword, passwordFocus] = useTextInput();
  const [checkEmail, setCheckEmail] = useState<string | undefined>(undefined);
  const [validEmail, setEmailValidity] = useState(true);
  const [validPassword, setPassValidity] = useState(true);
  const { t } = useStrings();

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const emailChangeDebouncer = useCallback(
    debounce((t: string) => {
      setCheckEmail(misspelledEmail(t));
      if (t === "") {
        setEmailValidity(true);
      } else {
        setEmailValidity(validFormat(t));
      }
    }, debounceTimeMs),
    [setEmail, setEmailValidity]
  );

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const passChangeDebouncer = useCallback(
    debounce((t: string) => {
      if (t === "") {
        setPassValidity(true);
      } else {
        setPassValidity(t.length >= 8);
      }
    }, debounceTimeMs),
    [setPassValidity]
  );

  const fieldsComplete = validFormat(email) && password.length >= 8;
  const tryLogin = () => {
    if (fieldsComplete && !props.inProgress) {
      props.login(email.trim(), password.trim());
    }
  };

  let emailErrorView = <></>;
  if (validEmail === false) {
    emailErrorView = (
      <ErrorBox
        testID="LoginForm ErrorBox EmailFormat"
        text={t.Formatting.EmailFormatFail}
        icon="Warning"
      />
    );
  }
  switch (props.emailError) {
    case "None":
      break;
    case "EmailFormat":
      emailErrorView = (
        <ErrorBox
          testID="LoginForm ErrorBox EmailFormat"
          text={t.Formatting.EmailFormatFail}
          icon="Warning"
        />
      );
      break;
    case "EmailMissing":
      emailErrorView = (
        <ErrorBox
          testID="LoginForm ErrorBox EmailMissing"
          text={t.LoginForm.EmailMissing}
          icon="Warning"
        />
      );
      break;
  }

  let emailFormatSuggestion = <></>;

  // help bad emails
  if (props.emailError === "None") {
    if (checkEmail === undefined) {
      // valid but no suggestion
      emailFormatSuggestion = <></>;
    } else {
      const changeEmail = () => {
        setEmail(checkEmail);
        setCheckEmail(undefined);
        void Analaytics.track("EmailSuggestion Accepted", {
          screen: "Signup",
          email: email,
          suggestion: checkEmail,
        });
      };
      emailFormatSuggestion = (
        <ErrorBox
          testID="LoginForm ErrorBox EmailSuggestion"
          text={t.Formatting.EmailSuggest(checkEmail)}
          icon="Warning"
          onPress={changeEmail}
        />
      );
    }
  }

  let passwordErrorView = <></>;
  if (validPassword === false) {
    passwordErrorView = (
      <ErrorBox
        testID="LoginForm ErrorBox PasswordRule"
        text={t.LoginForm.PasswordRuleFail}
        icon="Security"
      />
    );
  }
  switch (props.passwordError) {
    case "None":
      break;
    case "PasswordRule":
      passwordErrorView = (
        <ErrorBox
          testID="LoginForm ErrorBox PasswordRule"
          text={t.LoginForm.PasswordRuleFail}
          icon="Security"
        />
      );
      break;
    case "PasswordWrong":
      passwordErrorView = (
        <ErrorBox
          testID="LoginForm ErrorBox PasswordWrong"
          text={t.LoginForm.DoubleCheckPassword}
          icon="Security"
        />
      );
      break;
  }
  const buttons = (
    <>
      <Button
        testID="LoginForm ResetPassword"
        style="Secondary"
        inProgress={props.inProgress}
        text={t.LoginForm.buttons.resetPassword}
        onPress={() => {
          props.gotoPasswordForgot();
        }}
      />
      <ContactSupport
        style="Secondary"
        emailSubject="Need help on the login screen"
      />
      <Button
        testID={`LoginForm Login${!fieldsComplete ? " Disabled" : ""}`}
        disabled={!fieldsComplete}
        style="Primary"
        inProgress={props.inProgress}
        text={t.LoginForm.buttons.login}
        onPress={() => {
          tryLogin();
        }}
      />
    </>
  );

  return (
    <FormScreen
      title={t.LoginForm.ScreenTitle}
      navigation={{ type: "action", action: props.goBack }}
      buttons={buttons}
      testID="LoginForm FormScreen"
    >
      <Label>Email Address</Label>
      <TextInput
        testID="LoginForm Email"
        autoCompleteType={"email"}
        keyboardType={"email-address"}
        textContentType={"username"}
        autoCapitalize={"none"}
        onChangeText={(text) => {
          setEmailValidity(true);
          setEmail(text);
          emailChangeDebouncer(text);
        }}
        placeholder={"jordan@me.com"}
        returnKeyType={email.length > 0 && password.length > 0 ? "go" : "next"}
        onSubmitEditing={(e) => {
          if (email.length > 0 && password.length > 0) {
            tryLogin();
          }
          passwordFocus(e);
        }}
        ref={refEmail}
        value={email}
        onBlur={() => {
          emailChangeDebouncer.flush();
        }}
      />
      {emailErrorView}
      {emailFormatSuggestion}

      <Label>Password</Label>
      <TextInput
        testID="LoginForm Password"
        autoCompleteType={"password"}
        secureTextEntry={true}
        textContentType={"password"}
        autoCapitalize={"none"}
        onChangeText={(t: string) => {
          setPassValidity(true);
          passChangeDebouncer(t);
          setPassword(t);
        }}
        placeholder="8 or more characters"
        returnKeyType="go"
        ref={refPassword}
        onSubmitEditing={(e) => {
          if (email.length > 0 && password.length > 0) {
            tryLogin();
          }
          emailFocus(e);
        }}
        value={password}
        onBlur={() => {
          passChangeDebouncer.flush();
        }}
      />
      {passwordErrorView}
    </FormScreen>
  );
};

export default LoginForm;
