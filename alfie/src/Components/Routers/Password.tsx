/* eslint-disable @typescript-eslint/no-explicit-any */
import React from "react";
import { Route, Switch, useHistory } from "../../PlatformSpecific/react-router";

import TrackScreen from "../TrackScreen";
import NoRouteMatch from "../NoRouteMatch/NoRouteMatchHOC";
import PasswordForgot from "../PasswordReset/PasswordForgot";
import PasswordCodeSend from "../PasswordReset/PasswordCodeSendHOC";
import PasswordReset from "../PasswordReset/PasswordResetHOC";

export const forgotPasswordPath = "/app/password/forgot";
export const getPasswordCodePath = (medium: string): string =>
  `/app/password/sendcode/${medium}`;
export const resetPasswordPath = (medium: string, iden: string): string =>
  `/app/password/reset/${medium}/${iden}`;

export default function PasswordRouter(): JSX.Element {
  const history = useHistory();

  return (
    <Switch>
      <Route exact path={forgotPasswordPath}>
        <TrackScreen screen="PasswordForgot" />
        <PasswordForgot
          gotoSMSCode={() => history.push(getPasswordCodePath("sms"))}
          gotoEmailCode={() => history.push(getPasswordCodePath("email"))}
          goBack={() => history.goBack()}
        />
      </Route>

      <Route
        exact
        path={getPasswordCodePath(":medium")}
        render={({ match }) => {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
          const medium = match.params.medium;
          return (
            <>
              <TrackScreen screen="PasswordCodeSend" />
              <PasswordCodeSend
                gotoReset={(medium, iden) =>
                  history.push(resetPasswordPath(medium, iden))
                }
                medium={medium}
              />
            </>
          );
        }}
      />

      <Route
        exact
        path={resetPasswordPath(":medium", ":identifier")}
        render={({ match }) => {
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
          const medium = match.params.medium;
          // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access
          const identifier = match.params.identifier;
          return (
            <>
              <TrackScreen screen="PasswordReset" />
              <PasswordReset
                medium={medium}
                identifier={identifier}
                goHome={() => history.push("/app/login")}
              />
            </>
          );
        }}
      />

      <Route>
        <TrackScreen screen="Password NoMatch" />
        <NoRouteMatch router="Password" />
      </Route>
    </Switch>
  );
}
