import React, { useContext, useState } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen, { Navigation } from "../../../../Form/FormScreen";
import Button from "../../../../Button/Button";
import TextInput from "../../../../Form/TextInput";
import Label from "../../../../Form/Label";
import ErrorBox from "../../../../Form/ErrorBox";
import { useTextInput } from "../../../../Form/hooks";
import { PopUpContext, PopUpStyles, ShrugIcon } from "../../../../PopUp";
import { AmountError } from "../../../../../Actions/SignUp/BankAccount/SignUpVerifyManual";
import ContactSupportButton from "../../../../Button/ContactSupport";
import TextStyles from "../../../../Styling/Text";
import { ThemeContext } from "../../../../Theming/ThemeContext";

interface Props {
  bankName: null | string;
  accountName: null | string;
  submit: (amount: string) => void;
  inProgress: boolean;
  amountError: AmountError;
  goBack: () => void;
}

export const VerifyManualLink = (props: Props): JSX.Element => {
  const [amount, setAmount] = useTextInput();
  const { popUp } = useContext(PopUpContext);
  const [localError, setError] = useState(false);
  const theme = useContext(ThemeContext);

  const submit = () => {
    const num = Number(amount);

    if (isNaN(num) || amount.length === 0) {
      setError(true);
      return;
    } else {
      setError(false);
      props.submit(amount);
    }
  };

  const changeAmount = (t: string) => {
    setAmount(t);
    if (localError === true) {
      setError(false);
    }
  };

  let errorInfo = <></>;
  switch (props.amountError) {
    case "None":
      if (localError) {
        errorInfo = (
          <ErrorBox
            testID="VerifyManualLink ErrorBox Amount Wrong"
            icon="Warning"
            text="This doesn't look right"
          />
        );
      }
      break;

    case "Amount":
      errorInfo = (
        <ErrorBox
          testID="VerifyManualLink ErrorBox Amount Wrong"
          icon="Warning"
          text="This doesn't look right"
        />
      );
      break;

    case "Attempts":
      errorInfo = (
        <ErrorBox
          testID="VerifyManualLink ErrorBox Attempt Limit"
          icon="Security"
          text="Too many attempts, try later"
        />
      );
      break;

    case "Format":
      errorInfo = (
        <ErrorBox
          testID="VerifyManualLink ErrorBox Amount Wrong"
          icon="Warning"
          text="This doesn't look right"
        />
      );
      break;
  }

  const navigation: Navigation = { type: "action", action: props.goBack };

  return (
    <FormScreen
      testID="VerifyManualLink FormScreen"
      title="Verify deposit"
      buttons={
        <>
          <Text
            style={[
              TextStyles.fontWeight400,
              theme.textStyle,
              styles.helperPadding,
            ]}
          >
            You should see a small transaction from Pay Tgthr in your account at{" "}
            <Text style={[TextStyles.fontWeight600]}>{props.bankName}</Text>{" "}
            named{" "}
            <Text style={[TextStyles.fontWeight600]}>{props.accountName}</Text>.
          </Text>
          <Button
            testID="VerifyManualLink Continue"
            text="Verify"
            onPress={submit}
            style="Primary"
            inProgress={props.inProgress}
          />
          <ContactSupportButton
            emailSubject="I need help linking a bank account"
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
            <Text style={PopUpStyles.header}>What's this?</Text>
            <View style={PopUpStyles.line} />
            <Text style={PopUpStyles.text}>
              We sent a small amount to the account information you gave us. We
              need you to verify this amount so we know the account is yours.
            </Text>
          </>
        );
      }}
    >
      <Label>Deposit amount</Label>
      <TextInput
        testID="VerifyManualLink Amount"
        autoCompleteType="off"
        keyboardType="decimal-pad"
        textContentType="none"
        value={amount}
        onChangeText={changeAmount}
      />
      {errorInfo}
    </FormScreen>
  );
};

export default VerifyManualLink;

const styles = StyleSheet.create({
  helperPadding: {
    marginBottom: 10,
  },
});
