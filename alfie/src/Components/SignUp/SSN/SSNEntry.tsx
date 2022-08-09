/* eslint-disable react-native/no-raw-text */
import React, { useCallback, useState } from "react";
import { debounce } from "lodash";
import { defaultTo } from "lodash";

import { formatInput } from "../../Form/Formatter";
import FormScreen from "../../Form/FormScreen";
import Label from "../../Form/Label";
import ErrorBox from "../../Form/ErrorBox";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import { ssnVerifier } from "../../../Helpers/SSN";
import PopUp from "../PopUp/PopUp";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";
import FixedTextInput from "../../FixedTextInput/FixedTextInput";

export type Errors = null | "Format" | "ReUse";

interface Props {
  submit: (phoneNumber: string) => void;
  inProgress: boolean;
  ssn: string;
  showHelp?: boolean;
  error: Errors;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const ScreenName = "SSNEntry";

const SSNEntry = (props: Props): JSX.Element => {
  const [ssn, setSSN] = useState("");
  const [ssnValid, setSsnValidity] = useState(true);
  const [showHelp, setShow] = useState<boolean>(
    defaultTo(props.showHelp, false)
  );

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const ssnChanged = useCallback(
    debounce((t: string) => {
      if (t === "") {
        setSsnValidity(true);
      } else {
        setSsnValidity(ssnVerifier(t) !== null);
      }
    }, 2000),
    [setSsnValidity]
  );

  const normalizedSSN = formatInput(3, 5, "-")(ssn);

  let ssnError = <></>;
  if (ssnValid === false) {
    ssnError = (
      <ErrorBox
        testID="SignUpPII SSN Error Format"
        icon="Security"
        text="Can you double check this?"
      />
    );
  }
  switch (props.error) {
    case "Format":
      ssnError = (
        <ErrorBox
          testID="SignUpPII SSN Error Format"
          icon="Security"
          text="Can you double check this?"
        />
      );
      break;

    case "ReUse":
      ssnError = (
        <ErrorBox
          testID="SignUpPII SSN Error ReUSE"
          icon="Security"
          text="This SSN is already in use"
        />
      );
      break;

    case null:
      break;
  }

  const ssnHelpText = ssn.length < 0 ? "(••• •• ••••)" : "";

  let quickCheckValid = true;
  if (normalizedSSN.length === 11 && ssnVerifier(normalizedSSN) === null) {
    ssnError = (
      <ErrorBox
        testID="SignUpPII SSN Error Format"
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
              text="Why? Is this secure?"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "SSNEntry",
                });
              }}
            />
            <ProgressButton
              testID={`${ScreenName} Continue`}
              inProgress={props.inProgress}
              style="Primary"
              text="Next"
              onPress={() => {
                props.submit(ssn);
              }}
              disabled={!(quickCheckValid && ssn.length === 11)}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Label>{`Social security number ${ssnHelpText}`}</Label>
        <FixedTextInput
          placeholder="178-05-1120"
          onChangeText={(t) => {
            const normalized = t.replace(/(\D)/g, "");
            setSsnValidity(true);
            setSSN(formatInput(3, 5, "-")(t));
            ssnChanged(normalized);
          }}
        />
        {ssnError}
      </FormScreen>
      <PopUp
        headingText="Why do we need your social security number?"
        bodyText={
          "Our bank requires that we collect your social security number, because it is the most unique identifier you have and the most likely to confirm your identity.\n\nWe use enchanced AES-256 encryption wrapped with GCM using a 96-bit nonce size AEAD to protect your SSN from hackers, breaches, and misuse"
        }
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default SSNEntry;
