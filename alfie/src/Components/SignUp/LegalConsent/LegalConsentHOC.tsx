import React, { useCallback } from "react";
import { Linking } from "react-native";
import Alert from "../../../PlatformSpecific/Alert";
import { useSelector } from "react-redux";

import { State } from "../../../State/State";
import { useLegalConsent } from "../../../Actions/SignUp/signUpLegalConsent";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";

import UserStateRouter from "../../Routers/UserStateRouter";
import LegalConsent from "./LegalConsent";

interface Props {
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

const LegalConsentHOC = (props: Props): JSX.Element => {
  const { baseURL } = useSelector((state: State) => ({
    baseURL: state.baseURL,
  }));
  const { inProgress, submit: submitConsent, submitted } = useLegalConsent(
    baseURL
  );

  const wontAccept = useCallback(() => {
    Alert.alert(
      "It's a lawyer thing",
      "We can't issue you a Tgthr card unless you accept the agreements",
      [
        { text: "Ok" },
        {
          text: "Bye",
          style: "cancel",
          onPress: () => {
            void Linking.openURL("http://wikiroulette.co");
          },
        },
      ]
    );
  }, []);

  if (submitted) {
    return <UserStateRouter push />;
  }

  return (
    <LegalConsent
      inProgress={inProgress}
      submitConsent={submitConsent}
      wontAccept={wontAccept}
      trackEvent={Analytics.track}
      {...props}
    />
  );
};

export default LegalConsentHOC;
