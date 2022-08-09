import React, { useCallback, useState } from "react";
import Analaytics from "../../PlatformSpecific/SegmentAnalytcs";
import { debounce } from "lodash";
import { useKeyboard } from "@react-native-community/hooks";
import { StyleSheet, Text } from "react-native";

import { EmailError, PasswordError } from "../../Actions/Login";

import ErrorBox from "../Form/ErrorBox";
import FormScreen from "../Form/FormScreen";
import TextInput from "../Form/TextInput";
import Label from "../Form/Label";
import { useTextInput } from "../Form/hooks";
import ProgressButton from "../Button/ProgressButton";
import { useStrings } from "../../Strings/Strings";
import { misspelledEmail, validFormat } from "../../Helpers/Email";
import Carousel from "./Carousel";
import TextStyles from "../Styling/Text";

interface Props {
  inProgress: boolean;
  signup: (email: string, password: string) => void;
  emailError: EmailError;
  passwordError: PasswordError;
  goBack: () => void;
  screenIndex: number;
  screenCount: number;
}

const debouncTimeMs = 2000;

export const SignupForm = (props: Props): JSX.Element => {
  const [email, setEmail, refEmail, emailFocus] = useTextInput();
  const [password, setPassword, refPassword, passwordFocus] = useTextInput();
  const [checkEmail, setCheckEmail] = useState<string | undefined>(undefined);
  const [validEmail, setEmailValidity] = useState(true);
  const [validPassword, setPassValidity] = useState(true);
  const { t } = useStrings();
  const { keyboardShown } = useKeyboard();

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const emailChangeDebouncer = useCallback(
    debounce((t: string) => {
      setCheckEmail(misspelledEmail(t));
      if (t === "") {
        setEmailValidity(true);
      } else {
        setEmailValidity(validFormat(t));
      }
    }, debouncTimeMs),
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
    }, debouncTimeMs),
    [setPassValidity]
  );

  const fieldsComplete = validFormat(email) && password.length >= 8;
  const trySignup = () => {
    if (fieldsComplete && !props.inProgress) {
      props.signup(email.trim(), password.trim());
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
          icon="Security"
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

  let passwordInfoView = <></>;
  if (password.length === 0) {
    passwordInfoView = (
      <ErrorBox
        testID="LoginForm ErrorBox Info"
        text="Just needs to be longer than 8"
        icon="Warning"
      />
    );
  }

  const buttons = (
    <>
      <ProgressButton
        testID={`LoginForm SignUp${!fieldsComplete ? " Disabled" : ""}`}
        disabled={!fieldsComplete}
        style="Primary"
        inProgress={props.inProgress}
        text={t.SignUpForm.buttons.signup}
        onPress={() => {
          trySignup();
        }}
        index={props.screenIndex}
        count={props.screenCount}
      />
    </>
  );

  return (
    <FormScreen
      title={t.SignUpForm.ScreenTitle}
      navigation={{ type: "action", action: props.goBack }}
      buttons={buttons}
      testID="LoginForm FormScreen"
      style={styles.form}
    >
      <Label>Email Address</Label>
      <TextInput
        testID="LoginForm Email"
        autoCompleteType={"email"}
        keyboardType={"email-address"}
        textContentType={"username"}
        autoCapitalize={"none"}
        onChangeText={(text) => {
          //reset warning on typing
          setEmailValidity(true);

          setEmail(text);
          emailChangeDebouncer(text);
        }}
        placeholder={"jordan@me.com"}
        onSubmitEditing={passwordFocus}
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
          //reset warning on typing
          setPassValidity(true);
          passChangeDebouncer(t);
          setPassword(t);
        }}
        ref={refPassword}
        onSubmitEditing={emailFocus}
        value={password}
        onBlur={() => {
          passChangeDebouncer.flush();
        }}
        placeholder="••••••••"
      />
      {passwordErrorView}
      {passwordInfoView}
      {keyboardShown ? (
        <></>
      ) : (
        <Carousel
          trackTap={() => {
            void Analaytics.track("User IntroCarousel Scrolled");
          }}
          texts={[
            <Text key="total trx">
              Couples have made over{" "}
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                $950,000
              </Text>{" "}
              in purchases
            </Text>,
            <Text key="time to signup">
              It usually takes{" "}
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                5 min
              </Text>{" "}
              to finish signing up
            </Text>,
            <Text key="accounts made">
              Over{" "}
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                2,000+
              </Text>{" "}
              couples have signed up
            </Text>,
            <Text key="restaurants">
              Couples have spent over{" "}
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                $200,000+
              </Text>{" "}
              at restaurants
            </Text>,
            <Text key="groceries">
              Couples have spent over{" "}
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                $250,000+
              </Text>{" "}
              on groceries
            </Text>,
            <Text key="Netflix">
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                102 couples
              </Text>{" "}
              split Netflix this month
            </Text>,
            <Text key="common day">
              <Text style={[TextStyles.fontWeight600, styles.boldFont]}>
                Saturday
              </Text>{" "}
              is the most common day to split purchases
            </Text>,
          ]}
        />
      )}
    </FormScreen>
  );
};

export default SignupForm;

const styles = StyleSheet.create({
  boldFont: {
    fontSize: 18,
  },
  form: { overflow: "visible" },
});
