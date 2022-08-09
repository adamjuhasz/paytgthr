import React from "react";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import Label from "../Form/Label";
import TextInput from "../Form/TextInput";
import { useStrings } from "../../Strings/Strings";
import { useTextInput } from "../Form/hooks";

export type Mediums = "sms" | "email";

export interface Props {
  medium: Mediums;
  submit: (medium: Mediums, iden: string) => void;
  inProgress: boolean;
  goBack: () => void;
}

const PasswordCodeSend = (props: Props): JSX.Element => {
  const [iden, setIden, refIden] = useTextInput();
  const { t } = useStrings();

  const submit = () => props.submit(props.medium, iden);

  let idenValid = true;
  switch (props.medium) {
    case "sms":
      idenValid = iden.length === 10;
      break;

    case "email":
      idenValid = true;
      break;
  }

  return (
    <FormScreen
      navigation={{ type: "action", action: props.goBack }}
      title={t.PasswordCodeSend.title}
      buttons={
        <>
          <Button
            style="Primary"
            text={t.PasswordCodeSend.buttons.sendCode}
            onPress={submit}
            inProgress={props.inProgress}
            disabled={props.inProgress || !idenValid}
          />
        </>
      }
    >
      <Label>{t.PasswordCodeSend.labels[props.medium]}</Label>
      <TextInput
        testID="PasswordCodeSend iden"
        autoCompleteType={props.medium === "sms" ? "tel" : "email"}
        textContentType={props.medium === "sms" ? "telephoneNumber" : "none"}
        keyboardType={props.medium === "sms" ? "phone-pad" : "email-address"}
        autoCapitalize="none"
        onChangeText={(text) => {
          let normalized = text;
          switch (props.medium) {
            case "sms":
              normalized = text.replace(/\D/g, "");
              if (normalized.length === 11 && normalized[0] === "1") {
                normalized = normalized.slice(1);
              }
              break;

            case "email":
              break;
          }

          setIden(normalized);
        }}
        ref={refIden}
        value={iden}
      />
    </FormScreen>
  );
};

export default PasswordCodeSend;
