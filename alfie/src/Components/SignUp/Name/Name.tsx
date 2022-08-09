/* eslint-disable react-native/no-raw-text */
import React, { useState } from "react";

import FormScreen from "../../Form/FormScreen";
import TextInput from "../../Form/TextInput";
import Label from "../../Form/Label";
import { useTextInput } from "../../Form/hooks";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import PopUp from "../PopUp/PopUp";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

interface Props {
  firstName?: string;
  lastName?: string;
  submit: (firstName: string, lastName: string) => void;
  inProgress: boolean;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const SignUpName = (props: Props): JSX.Element => {
  const [firstName, setFirstName, refFName, focusFName] = useTextInput(
    props.firstName
  );
  const [lastName, setLastName, refLName, focusLName] = useTextInput(
    props.lastName
  );
  const [showHelp, setShow] = useState<boolean>(false);

  const bothFilled = firstName !== "" && lastName !== "";

  return (
    <>
      <FormScreen
        testID="SignUpName FormScreen"
        title="Legal name"
        navigation={{ type: "action", action: props.goBack }}
        buttons={
          <>
            <Button
              style="Secondary"
              text="Why do you need this?"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "Name",
                });
              }}
            />
            <ProgressButton
              testID="SignUpName Continue"
              inProgress={props.inProgress}
              disabled={!bothFilled}
              style="Primary"
              text="Next"
              onPress={() => {
                props.submit(firstName, lastName);
              }}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Label>First name</Label>
        <TextInput
          testID="SignUpName FirstName"
          autoCompleteType={"name"}
          textContentType={"givenName"}
          value={firstName}
          onChangeText={setFirstName}
          placeholder={"John"}
          ref={refFName}
          onSubmitEditing={focusLName}
        />
        <Label>Last name</Label>
        <TextInput
          testID="SignUpName LastName"
          autoCompleteType={"name"}
          textContentType={"familyName"}
          value={lastName}
          onChangeText={setLastName}
          placeholder={"Appleseed"}
          ref={refLName}
          onSubmitEditing={focusFName}
        />
      </FormScreen>
      <PopUp
        headingText="Why do we need your full legal name?"
        bodyText="We’re required to perform identity checks on all customers. We check your full legal name (along with the other information we’re collecting) against public records to make sure you’re real."
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default SignUpName;
