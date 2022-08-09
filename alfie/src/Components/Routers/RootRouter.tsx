/* eslint-disable @typescript-eslint/no-explicit-any */
import React, { useEffect } from "react";
import {
  Redirect,
  Route,
  Switch,
  useHistory,
  useLocation,
} from "../../PlatformSpecific/react-router";
import { Linking } from "react-native";
import console from "../../Global/Console";

import MainScreenRouter from "./MainScreenRouter";
import ReferralsRouter from "./ReferralsRouter";

import TrackScreen from "../TrackScreen";
import LoginForm from "../LoginForm/LoginFormHOC";
import SignUpRouter from "./SignUpRouter";
import SettingsRouter from "./SettingsRouter";
import NoRouteMatch from "../NoRouteMatch/NoRouteMatchHOC";
import UserClosed from "../AccountClosed/AccountClosed";
import LogViewer from "../Logs/LogViewer";
import PasswordRouter, { forgotPasswordPath } from "./Password";
import IntroRouter from "./IntroRouter";
import { loginPath, userClosedPath } from "./RootRouter/Paths";

const LinkOpener = () => {
  const location = useLocation<unknown>();

  useEffect(() => {
    setTimeout(() => {
      Linking.openURL(`https://paytgthr.com${location.pathname}`).catch(
        (err) => {
          console.error("Linking.openURL", err);
        }
      );
    }, 100);
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <Redirect to={{ pathname: "/", state: { dueTo: "<LinkOpener />" } }} />
  );
};

const RootRouter = (): JSX.Element => {
  const history = useHistory();

  return (
    <Switch>
      {/* well they got the app... */}
      <Route exact path="/getapp">
        <Redirect to="/" />
      </Route>

      <Route exact path="/app/logs">
        <LogViewer goBack={() => history.push("/")} />
      </Route>

      <Route path="/intro">
        <IntroRouter />
      </Route>

      <Route path="/legal">
        <LinkOpener />
      </Route>

      <Route exact path={loginPath}>
        <TrackScreen screen="Login" />
        <LoginForm
          goBack={() => history.goBack()}
          gotoPasswordForgot={() => history.push(forgotPasswordPath)}
        />
      </Route>

      <Route exact path={userClosedPath}>
        <TrackScreen screen="UserClosed" />
        <UserClosed />
      </Route>

      <Route path="/app/signup">
        <SignUpRouter />
      </Route>

      <Route path="/app/change">
        <SettingsRouter />
      </Route>

      <Route path="/app/card">
        <SettingsRouter />
      </Route>

      <Route path="/app/password">
        <PasswordRouter />
      </Route>

      <Route path="/app/referral">
        <ReferralsRouter />
      </Route>

      <Route path="/">
        <MainScreenRouter />
      </Route>

      <Route>
        <TrackScreen screen="RoutedLayer NoMatch" />
        <NoRouteMatch router="RoutedLayer" />
      </Route>
    </Switch>
  );
};

export default RootRouter;
