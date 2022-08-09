import React from "react";
import { Alert } from "react-native";

import { useLinkCode } from "../../Actions/Referral/LinkRefCode";

import LinkCode from "./LinkCode";

export const path = "/app/referral/link";

interface Props {
  goBack: () => void;
  gotoNext: () => void;
}

export default function LinkReferralCode(props: Props): JSX.Element {
  const [mutate, { isLoading, isError }] = useLinkCode();

  const sendCode = async (code: string) => {
    try {
      await mutate(code);
      props.gotoNext();
    } catch (e) {
      Alert.alert("That doesn't look right...");
    }
  };

  return (
    <LinkCode
      goBack={props.goBack}
      useReferralCode={sendCode}
      inProgress={isLoading}
      inError={isError}
    />
  );
}
