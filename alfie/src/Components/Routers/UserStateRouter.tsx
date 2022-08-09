import React, { useContext } from "react";
import { StyleSheet, Text } from "react-native";
import { useSelector } from "react-redux";
import { useQuery } from "react-query";
import { Redirect } from "../../PlatformSpecific/react-router";
import console from "../../Global/Console";

import { State } from "../../State/State";
import {
  getCurrentUserState,
  queryPath,
} from "../../Actions/User/GetUserState";
import pathForStep from "./SignUpRouter/Paths";
import { UserState } from "../../Types/UserStateTypes";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";
import { dashboardPath } from "./MainScreenRouter/Paths";

interface Props {
  background?: React.ReactNode;
  loading?: React.ReactNode;
  push: boolean;
}

const StateRouter = (props: Props): JSX.Element => {
  const { loggedIn } = useSelector((state: State) => ({
    loggedIn: state.userInfo.loggedIn,
  }));

  if (loggedIn === false) {
    return <Redirect to={pathForStep.createAccount} push={props.push} />;
  }

  return <UserStateRouter {...props} />;
};

const UserStateRouter = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));

  const query = useQuery([queryPath], getCurrentUserState(baseURL), {
    staleTime: 100,
  });

  const LoadingComponents = (): JSX.Element => {
    if (props.loading !== undefined) {
      return <>{props.loading}</>;
    } else if (props.background !== undefined) {
      return <>{props.background}</>;
    } else {
      return <theme.background style={[StyleSheet.absoluteFill]} />;
    }
  };
  const ErrorComponents = (): JSX.Element => {
    if (props.background !== undefined) {
      return <>{props.background}</>;
    } else {
      return (
        <>
          <theme.background style={[StyleSheet.absoluteFill]} />
          <Text style={[theme.textStyle, TextStyles.fontWeight400]}>Error</Text>
        </>
      );
    }
  };

  switch (query.status) {
    case "loading":
      return <LoadingComponents />;

    case "error":
      return <ErrorComponents />;

    default: {
      const data = query.data;
      if (data === undefined) {
        return <ErrorComponents />;
      }

      if (query.isStale === true) {
        return <LoadingComponents />;
      }

      const redirectPath = redirectTo(data);
      console.log("UserStateRouter", redirectPath, data);

      return <Redirect to={redirectPath} push={props.push} />;
    }
  }
};

export default StateRouter;

export const redirectTo = (state: UserState): string => {
  switch (state.user.status.tag) {
    case "UserCreated":
      break;

    case "UserWaitingOnPII":
      break;

    case "UserUpdated":
      break;

    case "UserClosed":
      return "/app/user/closed";

    case "UserActive":
      return dashboardPath;

    case "UserKYCDelay":
    case "UserWaitingOnKYC":
    case "UserUpdatedKYCDelay":
      return pathForStep.kyc;
  }

  if (state.user.phone.number === null) {
    return pathForStep.phoneEntry;
  }

  if (state.user.phone.verified === false) {
    return pathForStep.phoneVerify;
  }

  if (state.user.name.first === null || state.user.name.last === null) {
    return pathForStep.nameEntry;
  }

  if (state.user.consent === null || state.user.disclosure === null) {
    return pathForStep.legalAgree;
  }

  if (state.user.address.street === null || state.user.address.zip === null) {
    return pathForStep.addressEntry;
  }

  if (state.user.dob === null) {
    return pathForStep.dobEntry;
  }

  if (state.user.ssn.exists === false) {
    return pathForStep.ssnEntry;
  }

  if (
    state.user.kyc.status === null ||
    state.user.kyc.status.kind !== "Passed"
  ) {
    return pathForStep.kyc;
  }

  // sign up complete
  return dashboardPath;
};
