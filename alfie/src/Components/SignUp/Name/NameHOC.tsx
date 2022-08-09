/* eslint-disable react-native/no-raw-text */
import React, { useContext, useEffect, useState } from "react";
import { ActivityIndicator, Text } from "react-native";
import { useSelector } from "react-redux";
import { useQuery } from "react-query";

import { State } from "../../../State/State";
import FormScreen from "../../Form/FormScreen";
import { getName, useSubmitName } from "../../../Actions/SignUp/SignUpName";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import TextStyles from "../../Styling/Text";
import UserStateRouter from "../../Routers/UserStateRouter";
import SignUpName from "./Name";
import { ThemeContext } from "../../Theming/ThemeContext";

interface Props {
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const SignUpNameHOC = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const { inProgress, submit, success } = useSubmitName(baseURL);
  const { status, data } = useQuery(["/app/signup/name"], getName(baseURL));
  const [fname, setFName] = useState<string>();
  const [lname, setLName] = useState<string>();

  useEffect(() => {
    if (status === "success" && data !== undefined) {
      if (data["fname-value"] !== null) {
        setFName(data["fname-value"]);
      }
      if (data["lname-value"] !== null) {
        setLName(data["lname-value"]);
      }
    }
  }, [status, data, setFName, setLName]);

  const loadingScreen = (
    <FormScreen
      title="Legal name"
      navigation={{ type: "none" }}
      buttons={<></>}
    >
      <ActivityIndicator color={theme.textColor} size="large" />
    </FormScreen>
  );

  if (success) {
    return <UserStateRouter loading={loadingScreen} push />;
  }

  switch (status) {
    case "loading": {
      return loadingScreen;
    }
    case "error": {
      return (
        <FormScreen
          title="Legal name"
          navigation={{ type: "none" }}
          buttons={<></>}
        >
          <Text
            style={[
              theme.textStyle,
              TextStyles.fontWeight400,
              TextStyles.centered,
              TextStyles.normalText,
            ]}
          >
            Uh Oh, houston we have a problem
          </Text>
        </FormScreen>
      );
    }

    default:
      return (
        <SignUpName
          inProgress={inProgress}
          submit={submit}
          firstName={fname}
          lastName={lname}
          trackEvent={Analytics.track}
          {...props}
        />
      );
  }
};

export default SignUpNameHOC;
