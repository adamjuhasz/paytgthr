/* eslint-disable react-native/no-raw-text */
import React, { useEffect, useState } from "react";
import { StyleSheet, Text } from "react-native";
import { defaultTo } from "lodash";

import FormScreen from "../../Form/FormScreen";
import TextInput from "../../Form/TextInput";
import Label from "../../Form/Label";
import ErrorBox from "../../Form/ErrorBox";
import { useTextInput } from "../../Form/hooks";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import PopUp from "../PopUp/PopUp";
import TextStyles from "../../Styling/Text";
import { darkOpacityWhite } from "../../Styling/Colors";
import { formatInput } from "../../Form/Formatter";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

interface Props {
  submit: (code: string) => void;
  gotoChangePhone: () => void;
  inProgress: boolean;
  phone: string;
  error: boolean;
  showHelp?: boolean;
  sendCode: () => Promise<void>;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const ScreenName = "PhoneVerify";

const PhoneVerify = (props: Props): JSX.Element => {
  const [emailCode, setEmailCode, refEmail] = useTextInput();
  const [canSendCode, resetSendCode] = useState(false);
  const [showHelp, setShow] = useState<boolean>(
    defaultTo(props.showHelp, true)
  );

  useEffect(() => {
    void props.sendCode();
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  useEffect(() => {
    let stillMounted = true;

    if (canSendCode === false) {
      setTimeout(() => {
        if (stillMounted) {
          resetSendCode(true);
        }
      }, 10000);
    }

    return () => {
      stillMounted = false;
    };
  }, [canSendCode]);
  const normalizedPhone = formatInput(3, 6, "-")(props.phone);

  const errorInfo = props.error ? (
    <ErrorBox
      icon="Security"
      text="That code doesn't seem right, can you double check it?"
    />
  ) : (
    <></>
  );

  return (
    <>
      <FormScreen
        testID={`${ScreenName} FormScreen`}
        title="Verify phone"
        navigation={{ type: "action", action: props.goBack }}
        buttons={
          <>
            <Button
              testID={`${ScreenName} Change`}
              inProgress={props.inProgress}
              style="Secondary"
              text="Change number"
              onPress={props.gotoChangePhone}
            />
            <Button
              testID={`${ScreenName} Send Code`}
              inProgress={props.inProgress}
              disabled={canSendCode === false}
              style="Secondary"
              text="Send a new code"
              onPress={() => {
                resetSendCode(false);
                void props.sendCode();
              }}
            />
            <Button
              style="Secondary"
              text="Why do you need this?"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "PhoneVerify",
                });
              }}
            />
            <ProgressButton
              testID={`${ScreenName} Continue`}
              inProgress={props.inProgress}
              style="Primary"
              text="Verify"
              onPress={() => {
                props.submit(emailCode);
              }}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Label>Code</Label>
        <TextInput
          testID={`${ScreenName} PhoneCode`}
          keyboardType="number-pad"
          textContentType="oneTimeCode"
          autoCapitalize={"none"}
          value={emailCode}
          onChangeText={setEmailCode}
          ref={refEmail}
        />
        {errorInfo}
        <Text style={[TextStyles.normalText, styles.emailConfirmText]}>
          Phone verification code sent to {normalizedPhone}
        </Text>
      </FormScreen>
      <PopUp
        headingText="We need to verify your phone number"
        bodyText={`We'll send a verification code to ${normalizedPhone}.\n\nIt may take up to 30 seconds to arrive`}
        buttonText="Send code"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default PhoneVerify;

const styles = StyleSheet.create({
  emailConfirmText: { color: darkOpacityWhite },
});
