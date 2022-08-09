import React from "react";
import { useSelector } from "react-redux";

import { State } from "../../../State/State";
import Dashboard from "./Dashboard";

interface Props {
  gotoIntro: () => void;
  gotoSignup: () => void;
}

export default function DashboardDemo(props: Props): JSX.Element {
  const { hasSeenIntro } = useSelector((state: State) => ({
    hasSeenIntro: state.seenIntro,
  }));

  return (
    <Dashboard
      hasSeenIntro={hasSeenIntro}
      hasVerifiedIdentity={false}
      hasInvitedPartner={false}
      partnerAccepted={false}
      haslinkedBank={false}
      hasVerifiedBank={false}
      setLoadedGift={() => {
        return;
      }}
      gotoAcceptCode={() => {
        return;
      }}
      gotoInviteCode={() => {
        return;
      }}
      gotoLinkBank={() => {
        return;
      }}
      gotoVerifyBank={() => {
        return;
      }}
      gotoPurhcase={() => {
        return;
      }}
      gotoSignup={props.gotoSignup}
      gotoIntro={props.gotoIntro}
      userState={null}
      userCreatedOn={null}
      partnerLinked={false}
      partnerVerified={false}
      partnerName={null}
    />
  );
}
