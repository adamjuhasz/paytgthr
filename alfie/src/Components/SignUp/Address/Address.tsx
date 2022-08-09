import React, { useState } from "react";
import { defaultTo } from "lodash";

import FormScreen from "../../Form/FormScreen";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import TextInput from "../../Form/TextInput";
import Label from "../../Form/Label";
import ErrorBox from "../../Form/ErrorBox";
import { useTextInput } from "../../Form/hooks";
import { Address, AddressErrors } from "../../../Actions/SignUp/SignUpAddress";
import PopUp from "../PopUp/PopUp";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

const all = (vals: string[]) =>
  vals.reduce((accum, curr) => accum && curr !== "", true);

interface Props {
  inProgress: boolean;
  submit: (address: Address) => void;
  errors: AddressErrors;
  street?: string;
  apt?: string;
  city?: string;
  state?: string;
  zip?: string;
  showHelp?: boolean;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

export const SignUpAddress = (props: Props): JSX.Element => {
  const [street, setStreet, streetRef, focusStreet] = useTextInput(
    props.street
  );
  const [apt, setApt, aptRef, focusApt] = useTextInput(props.apt);
  const [city, setCity, cityRef, focusCity] = useTextInput(props.city);
  const [state, setState, stateRef, focusState] = useTextInput(props.state);
  const [zip, setZip, zipRef, focusZip] = useTextInput(props.zip);
  const [showHelp, setShow] = useState<boolean>(
    defaultTo(props.showHelp, false)
  );

  const allDone = all([street, city, state, zip]);

  let streetError = <></>;
  if (props.errors.street !== "None") {
    streetError = (
      <ErrorBox icon="Security" text="Can you double check this?" />
    );
  }

  let cityError = <></>;
  if (props.errors.city !== "None") {
    cityError = <ErrorBox icon="Security" text="Can you double check this?" />;
  }

  let stateError = <></>;
  if (props.errors.state !== "None") {
    stateError = <ErrorBox icon="Security" text="Can you double check this?" />;
  }

  let zipError = <></>;
  if (props.errors.zip !== "None") {
    zipError = <ErrorBox icon="Security" text="Can you double check this?" />;
  }

  const submit = () => {
    props.submit({
      street,
      apt,
      city,
      state,
      zip,
    });
  };

  return (
    <>
      <FormScreen
        testID="SignUpAddress FormScreen"
        title="Residential address"
        navigation={{ type: "action", action: props.goBack }}
        buttons={
          <>
            <Button
              text="Why do you need this?"
              style="Secondary"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "Address",
                });
              }}
            />
            <ProgressButton
              testID="SignUpAddress Continue"
              text="Next"
              style="Primary"
              inProgress={props.inProgress}
              disabled={!allDone}
              onPress={submit}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Label>Street</Label>
        <TextInput
          testID="SignUpAddress StreetAddressLine1"
          autoCompleteType="street-address"
          textContentType="streetAddressLine1"
          autoCorrect={false}
          keyboardType="default"
          autoCapitalize="words"
          value={street}
          onChangeText={setStreet}
          placeholder="123 Main Street"
          ref={streetRef}
          onSubmitEditing={focusApt}
        />
        {streetError}

        <Label>Apt #</Label>
        <TextInput
          testID="SignUpAddress StreetAddressLine2"
          textContentType="streetAddressLine2"
          keyboardType="default"
          autoCapitalize="words"
          value={apt}
          onChangeText={setApt}
          placeholder="Apt 5R"
          ref={aptRef}
          onSubmitEditing={focusCity}
        />

        <Label>City</Label>
        <TextInput
          testID="SignUpAddress AddressCity"
          textContentType="addressCity"
          keyboardType="default"
          autoCapitalize="words"
          autoCorrect={false}
          value={city}
          onChangeText={setCity}
          placeholder="San Francisco"
          ref={cityRef}
          onSubmitEditing={focusState}
        />
        {cityError}

        <Label>State</Label>
        <TextInput
          testID="SignUpAddress AddressState"
          textContentType="addressState"
          keyboardType="default"
          autoCapitalize="characters"
          autoCorrect={false}
          value={state}
          onChangeText={(t) => {
            setState(t.toUpperCase().slice(0, 2));
          }}
          placeholder="CA"
          ref={stateRef}
          onSubmitEditing={focusZip}
        />
        {stateError}

        <Label>Zip</Label>
        <TextInput
          testID="SignUpAddress PostalCode"
          autoCompleteType="postal-code"
          textContentType="postalCode"
          keyboardType="number-pad"
          autoCorrect={false}
          value={zip}
          onChangeText={(t) => {
            setZip(t.slice(0, 5));
          }}
          placeholder="94103"
          ref={zipRef}
          onSubmitEditing={focusStreet}
        />
        {zipError}
      </FormScreen>
      <PopUp
        headingText="Why do we need your address?"
        bodyText="We match your address along with your name and the other information we’re collecting to verify your identity. Also, in the future, we’ll be offering physical Tgthr Cards, and we’ll need to know where to send it! Unfortunately, we can’t accept PO boxes, so please use your home address."
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};

export default SignUpAddress;
