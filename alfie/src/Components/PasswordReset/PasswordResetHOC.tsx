import React, { useEffect } from "react";
import { useSelector } from "react-redux";
import Analytics from "../../PlatformSpecific/SegmentAnalytcs";
import { useHistory } from "../../PlatformSpecific/react-router";

import { State } from "../../State/State";
import PasswordReset, { Mediums } from "./PasswordReset";
import { useResetPassword } from "../../Actions/User/ResetPassword";

interface Props {
  goHome: () => void;
  medium: Mediums;
  identifier: string;
}

const PasswordResetHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const { submit, state, errors } = useResetPassword(baseURL);
  const history = useHistory();

  const changePassword = (token: string, newPassword: string) => {
    void Analytics.track("Password Change Requested", { medium: props.medium });
    void submit({
      medium: props.medium,
      token,
      identifier: props.identifier,
      newPassword,
    }).then(() => {
      void Analytics.track("Password Reset Successful", {
        medium: props.medium,
      });
    });
  };

  useEffect(() => {
    if (state === "submitted") {
      props.goHome();
    }
  }, [state, props]);

  switch (state) {
    case "none":
    case "error":
      return (
        <PasswordReset
          inProgress={false}
          submit={changePassword}
          passwordError={errors.passwordFailure}
          tokenExpired={
            errors.tokenFailure === "TokenExpired" ||
            errors.tokenFailure === "TokenAlreadyUsed"
          }
          tokenFailure={errors.tokenFailure === "TokenNotFound"}
          goBack={() => history.goBack()}
        />
      );

    case "inProgress":
    case "submitted":
      return (
        <PasswordReset
          inProgress={true}
          submit={changePassword}
          passwordError={errors.passwordFailure}
          tokenExpired={
            errors.tokenFailure === "TokenExpired" ||
            errors.tokenFailure === "TokenAlreadyUsed"
          }
          tokenFailure={errors.tokenFailure === "TokenNotFound"}
          goBack={() => history.goBack()}
        />
      );
  }
};

export default PasswordResetHOC;
