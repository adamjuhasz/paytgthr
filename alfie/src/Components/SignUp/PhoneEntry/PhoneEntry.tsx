/* eslint-disable react-native/no-raw-text */
import React, { useCallback, useState } from "react";
import { debounce } from "lodash";
import { Platform } from "react-native";
import { defaultTo } from "lodash";

import { formatInput, limitInput } from "../../Form/Formatter";
import FormScreen from "../../Form/FormScreen";
import TextInput from "../../Form/TextInput";
import Label from "../../Form/Label";
import ErrorBox from "../../Form/ErrorBox";
import { useTextInput } from "../../Form/hooks";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import { phoneVerifier } from "../../../Helpers/Phone";
import PopUp from "../PopUp/PopUp";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

interface Props {
  submit: (phoneNumber: string) => void;
  inProgress: boolean;
  phone: string;
  showHelp?: boolean;
  error: null | "Format";
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const ScreenName = "PhoneEntry";

const PhoneEntry = (props: Props): JSX.Element => {
  const [phone, setPhone, refPhone] = useTextInput(props.phone);
  const [phoneValid, setPhoneValidity] = useState(true);
  const [showHelp, setShow] = useState<boolean>(
    defaultTo(props.showHelp, false)
  );

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const phoneChanged = useCallback(
    debounce((t: string) => {
      if (t === "") {
        setPhoneValidity(true);
      } else {
        setPhoneValidity(phoneVerifier(t) !== null);
      }
    }, 2000),
    [setPhoneValidity]
  );

  const normalizedPhone = formatInput(3, 6, "-")(phone);

  let phoneError = <></>;
  if (phoneValid === false) {
    phoneError = (
      <ErrorBox
        testID="SignUpPII Phone Error Format"
        icon="Security"
        text="Can you double check this?"
      />
    );
  }
  switch (props.error) {
    case "Format":
      phoneError = (
        <ErrorBox
          testID="SignUpPII Phone Error Format"
          icon="Security"
          text="Can you double check this?"
        />
      );
      break;

    case null:
      break;
  }

  if (
    normalizedPhone.length === 12 &&
    phoneVerifier(normalizedPhone) === null
  ) {
    phoneError = (
      <ErrorBox
        testID="SignUpPII Phone Error Format"
        icon="Security"
        text="Can you double check this?"
      />
    );
  }

  const phoneHelpText = phone.length > 0 ? "(●●●-●●●-●●●●)" : "";

  return (
    <>
      <FormScreen
        testID={`${ScreenName} FormScreen`}
        title="Phone number"
        navigation={{ type: "action", action: props.goBack }}
        buttons={
          <>
            <Button
              style="Secondary"
              text="Why do you need this?"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "PhonEntry",
                });
              }}
            />
            <ProgressButton
              testID={`${ScreenName} Continue`}
              inProgress={props.inProgress}
              style="Primary"
              text="Next"
              onPress={() => {
                props.submit(phone);
              }}
              disabled={phone.length !== 12}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Label>{`Phone number ${phoneHelpText}`}</Label>
        <TextInput
          testID="SignUpPII Phone"
          autoCompleteType={"tel"}
          textContentType={"telephoneNumber"}
          keyboardType={Platform.OS === "web" ? "default" : "phone-pad"}
          value={normalizedPhone}
          onChangeText={(t) => {
            setPhoneValidity(true);
            limitInput(setPhone, 10)(t);
            phoneChanged(t);
          }}
          placeholder={"415-867-5309"}
          ref={refPhone}
          onBlur={() => {
            phoneChanged.flush();
          }}
        />
        {phoneError}
      </FormScreen>
      <PopUp
        headingText="Why do we need your phone number?"
        bodyText="The Tgthr Card is a virtual card delivered directly to your phone. We’ll need to make sure that we’re sending it to the right number. We’ll send a confirmation text to make sure we’re all set."
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default PhoneEntry;
