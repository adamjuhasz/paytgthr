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
import { dobVerifier } from "../../../Helpers/DOB";
import PopUp from "../PopUp/PopUp";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

interface Props {
  submit: (phoneNumber: string) => void;
  inProgress: boolean;
  dob: string;
  showHelp?: boolean;
  error: null | "Format";
  usersName: string;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const ScreenName = "DOBEntry";

const DOBEntry = (props: Props): JSX.Element => {
  const [dob, setDob, refDob] = useTextInput(props.dob);
  const [dobValid, setDobValidity] = useState(true);
  const [showHelp, setShow] = useState<boolean>(
    defaultTo(props.showHelp, false)
  );

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const dobChanged = useCallback(
    debounce((t: string) => {
      if (t === "") {
        setDobValidity(true);
      } else {
        setDobValidity(dobVerifier(t) !== null);
      }
    }, 2000),
    [setDobValidity]
  );

  const normalizedDob = formatInput(2, 4, "/")(dob);

  let dobError = <></>;
  if (dobValid === false) {
    dobError = (
      <ErrorBox
        testID="SignUpPII DOB Error Format"
        icon="Security"
        text="Can you double check this?"
      />
    );
  }
  switch (props.error) {
    case "Format":
      dobError = (
        <ErrorBox
          testID="SignUpPII DOB Error Format"
          icon="Security"
          text="Can you double check this?"
        />
      );
      break;

    case null:
      break;
  }

  const dobHelpText = dob.length > 0 ? "(●●/●●/●●●●)" : "";

  let quickCheckValid = true;
  if (normalizedDob.length === 10 && dobVerifier(normalizedDob) === null) {
    dobError = (
      <ErrorBox
        testID="SignUpPII DOB Error Format"
        icon="Security"
        text="Can you double check this?"
      />
    );
    quickCheckValid = false;
  }

  return (
    <>
      <FormScreen
        testID={`${ScreenName} FormScreen`}
        title="Identity verification"
        navigation={{ type: "action", action: props.goBack }}
        buttons={
          <>
            <Button
              style="Secondary"
              text="Why do you need this?"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "DOBEntry",
                });
              }}
            />
            <ProgressButton
              testID={`${ScreenName} Continue`}
              inProgress={props.inProgress}
              style="Primary"
              text="Next"
              onPress={() => {
                props.submit(dob);
              }}
              disabled={!(quickCheckValid && dob.length === 10)}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Label>{`Date of birth ${dobHelpText}`}</Label>
        <TextInput
          testID="SignUpPII DOB"
          spellCheck={false}
          autoCompleteType={"off"}
          textContentType={"none"}
          keyboardType={Platform.OS === "web" ? "default" : "number-pad"}
          value={normalizedDob}
          onChangeText={(t) => {
            setDobValidity(true);
            limitInput(setDob, 8)(t);
            dobChanged(t);
          }}
          placeholder={"08/21/1989"}
          ref={refDob}
          onBlur={() => {
            dobChanged.flush();
          }}
        />
        {dobError}
      </FormScreen>
      <PopUp
        headingText="Why do we need your date of birth? "
        bodyText={`There might be a lot of people with the same name out there! We want to make sure we're interacting with the right one.`}
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default DOBEntry;
