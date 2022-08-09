import React from "react";
import { Linking } from "react-native";
import { useSelector } from "react-redux";

import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";
import { State } from "../../../State/State";
import Survey from "./Survey";

interface Props {
  gotoNextScreen: () => void;
}

const SurveyHOC = (props: Props): JSX.Element => {
  const { uid, email } = useSelector((state: State) => ({
    uid: state.userInfo.userId,
    email: state.userInfo.email,
  }));

  const gotoNextScreen = () => {
    void Analytics.track("Signup Survey Declined");
    props.gotoNextScreen();
  };

  const openSurvey = () => {
    void Analytics.track("Signup Survey Opened");
    let userQS = "";
    if (uid !== null) {
      userQS = `&userId=${uid}`;
    }

    let emailQS = "";
    if (email !== null) {
      emailQS = `&email=${email}`;
    }

    void Linking.openURL(
      `https://app.satismeter.com/survey?token=bIVwNMzwNTEJYwEI&campaign=60d10cb67b6491c3e1512b0e${userQS}${emailQS}`
    );
    props.gotoNextScreen();
  };

  return <Survey gotoNext={gotoNextScreen} gotoSurvey={openSurvey} />;
};

export default SurveyHOC;
