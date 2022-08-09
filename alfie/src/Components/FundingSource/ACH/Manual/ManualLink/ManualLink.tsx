import React, { useContext, useEffect, useState } from "react";
import { Text, View } from "react-native";
import { capitalize } from "lodash";

import FormScreen, { Navigation } from "../../../../Form/FormScreen";
import Button from "../../../../Button/Button";
import TextInput from "../../../../Form/TextInput";
import Label from "../../../../Form/Label";
import { useTextInput } from "../../../../Form/hooks";
import { SubmitErrors } from "../../../../../Actions/SignUp/BankAccount/SignUpManualLink";
import { PopUpContext, PopUpStyles, ShrugIcon } from "../../../../PopUp";
import ContactSupport from "../../../../Button/ContactSupport";
import ErrorBox from "../../../../Form/ErrorBox";

interface Props {
  submit: (
    accountNumber: string,
    routingNumber: string,
    accountName: string,
    bankName: string
  ) => void;
  inProgress: boolean;
  errors: SubmitErrors;
  goBack: () => void;
  routingList: Record<string, string>;
}

export const ManualLink = (props: Props): JSX.Element => {
  const [bankName, setBank, refBank, bankFocus] = useTextInput();
  const [accountName, setAccountName, refAccountName, accountNameFocus] =
    useTextInput();
  const [
    accountNumber,
    setAccountNumber,
    refAccountNumber,
    accountNumberFocus,
  ] = useTextInput();
  const [
    routingNumber,
    setRoutingNumber,
    refRoutingNumber,
    routingNumberFocus,
  ] = useTextInput();
  const { popUp } = useContext(PopUpContext);
  const [buttonDisabled, disableButton] = useState(true);
  const [routingError, setRoutingError] = useState<
    null | "length" | "nonnumber"
  >(null);
  const [routingWarning, setRoutingWarning] = useState<
    null | "length" | "mismatch"
  >(null);
  const [accountError, setAccountError] = useState<
    null | "tooshort" | "toolong" | "nonnumber"
  >(null);

  const submit = () => {
    props.submit(accountNumber, routingNumber, accountName, bankName);
  };

  const changeRouting = (text: string) => {
    if (routingNumber.length >= 9 && text.length < 9) {
      setRoutingWarning("length");
    } else {
      setRoutingWarning(null);
    }

    if (text.length > 9) {
      setRoutingError("length");
    } else {
      setRoutingError(null);
    }

    if (text.length === 9) {
      if (/^\d+$/.exec(text) === null) {
        setRoutingError("nonnumber");
      } else {
        if (props.routingList[text] !== undefined) {
          setBank(capitalize(props.routingList[text]));
        } else {
          setRoutingWarning("mismatch");
        }
      }
    }

    setRoutingNumber(text);
  };

  const changeAccountNumber = (text: string) => {
    if (/^\d+$/.exec(text) === null) {
      setAccountError("nonnumber");
    } else {
      setAccountError(null);
    }

    setAccountNumber(text);
  };

  useEffect(() => {
    disableButton(
      bankName.length === 0 ||
        accountName.length === 0 ||
        accountNumber.length < 4 ||
        accountNumber.length > 17 ||
        routingNumber.length !== 9 ||
        /^\d+$/.exec(routingNumber) === null ||
        /^\d+$/.exec(accountNumber) === null
    );
  }, [bankName, accountName, accountNumber, routingNumber, disableButton]);

  const navigation: Navigation = { type: "action", action: props.goBack };

  return (
    <FormScreen
      testID="ManualLink FormScreen"
      title="Manual link"
      buttons={
        <>
          <Button
            testID="ManualLink Continue"
            text="Continue"
            onPress={submit}
            style="Primary"
            inProgress={props.inProgress}
            disabled={buttonDisabled}
          />
          <ContactSupport
            emailSubject="How do I manually link an account?"
            style="Secondary"
          />
        </>
      }
      navigation={navigation}
      rightIcon={<ShrugIcon />}
      rightAction={() => {
        popUp(
          "Linking help",
          <>
            <Text style={PopUpStyles.header}>Why do we need this?</Text>
            <View style={PopUpStyles.line} />
            <Text style={PopUpStyles.text}>
              We'll micro-credit your account along with a corresponding
              micro-deposit. You'll need to enter this amount, so we can verify
              that you control the bank account and aren't linking another
              person's account.
            </Text>
          </>
        );
      }}
    >
      <Label>Account name</Label>
      <TextInput
        testID="ManualLink Account Name"
        autoCompleteType="off"
        autoCapitalize="words"
        textContentType="none"
        value={accountName}
        ref={refAccountName}
        onSubmitEditing={routingNumberFocus}
        onChangeText={setAccountName}
        placeholder="John's checking"
      />

      <Label>Routing number</Label>
      <TextInput
        testID="ManualLink Routing Number"
        autoCompleteType="off"
        keyboardType="number-pad"
        textContentType="none"
        value={routingNumber}
        ref={refRoutingNumber}
        onSubmitEditing={accountNumberFocus}
        onChangeText={changeRouting}
        placeholder="123456789"
      />
      {routingError === "length" ? (
        <ErrorBox
          icon="Warning"
          text="This seems too long, can you double check this?"
        />
      ) : undefined}
      {routingError === "nonnumber" ? (
        <ErrorBox icon="Security" text="Can you double check this?" />
      ) : undefined}
      {routingWarning === "mismatch" ? (
        <ErrorBox
          icon="Warning"
          text="We don't recognize this bank, can you double check it?"
        />
      ) : undefined}
      {routingWarning === "length" ? (
        <ErrorBox
          icon="Warning"
          text="This seems too short, can you double check this?"
        />
      ) : undefined}

      <Label>Account number</Label>
      <TextInput
        testID="ManualLink Account Number"
        autoCompleteType="off"
        keyboardType="number-pad"
        textContentType="none"
        value={accountNumber}
        ref={refAccountNumber}
        onSubmitEditing={bankFocus}
        onChangeText={changeAccountNumber}
        placeholder="0"
      />
      {accountError === "tooshort" ? (
        <ErrorBox
          icon="Warning"
          text="This seems too short, can you double check this?"
        />
      ) : undefined}
      {accountError === "toolong" ? (
        <ErrorBox
          icon="Warning"
          text="This seems too long, can you double check this?"
        />
      ) : undefined}
      {accountError === "nonnumber" ? (
        <ErrorBox
          icon="Warning"
          text="This seems wrong, can you double check this?"
        />
      ) : undefined}

      <Label>Bank name</Label>
      <TextInput
        testID="ManualLink Bank Name"
        autoCompleteType="off"
        autoCapitalize="words"
        textContentType="none"
        value={bankName}
        ref={refBank}
        onSubmitEditing={accountNameFocus}
        onChangeText={setBank}
        placeholder="Community Bank of CA"
      />
      {props.errors.metaError === "Attempts" ? (
        <ErrorBox
          icon="Security"
          text="Too many attempts, can you get in touch with us?"
        />
      ) : (
        <></>
      )}
    </FormScreen>
  );
};

export default ManualLink;
