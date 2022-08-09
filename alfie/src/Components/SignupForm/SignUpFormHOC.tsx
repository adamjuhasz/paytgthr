import React from "react";
import { Alert } from "react-native";

import { useLogin } from "../../Actions/Login";
import UserStateRouter from "../Routers/UserStateRouter";
import SignUpForm from "./SignUpForm";

interface Props {
  goBack: () => void;
  screenIndex: number;
  screenCount: number;
}

const SignUpFormHOC = (props: Props): JSX.Element => {
  const { inProgress, emailErrors, passwordErrors, success } = useLogin();

  if (success) {
    return <UserStateRouter push={false} />;
  }

  return (
    <SignUpForm
      inProgress={inProgress}
      signup={() => {
        Alert.alert(
          "Thanks for all the fish!",
          "Pay Tgthr is closing up shop in March"
        );
        return;
      }}
      emailError={emailErrors}
      passwordError={passwordErrors}
      goBack={props.goBack}
      screenIndex={props.screenIndex}
      screenCount={props.screenCount}
    />
  );
};

export default SignUpFormHOC;
