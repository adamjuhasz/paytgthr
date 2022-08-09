import React from "react";
import { useDispatch } from "react-redux";

import { AlfieDispatch } from "../../State/Store";
import { login, useLogin } from "../../Actions/Login";
import UserStateRouter from "../Routers/UserStateRouter";
import LoginForm from "./LoginForm";

interface HOCProps {
  gotoPasswordForgot: () => void;
  goBack: () => void;
}

const LoginFormHOC = (props: HOCProps): JSX.Element => {
  const dispatch = useDispatch<AlfieDispatch>();
  const loginFn = (email: string, password: string) =>
    dispatch(login(email, password));

  const {
    submit: doLogin,
    inProgress,
    emailErrors,
    passwordErrors,
    success,
  } = useLogin();

  if (success) {
    return <UserStateRouter push={false} />;
  }

  return (
    <LoginForm
      inProgress={inProgress}
      login={(e, p) => doLogin(loginFn, e, p)}
      emailError={emailErrors}
      passwordError={passwordErrors}
      gotoPasswordForgot={props.gotoPasswordForgot}
      goBack={props.goBack}
    />
  );
};

export default LoginFormHOC;
