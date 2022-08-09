import React from "react";
import { useDispatch, useSelector } from "react-redux";
import Analytics from "../../../PlatformSpecific/SegmentAnalytcs";
import { defaultTo } from "lodash";
import { Redirect } from "../../../PlatformSpecific/react-router";

import { State } from "../../../State/State";
import { AlfieDispatch } from "../../../State/Store";

import useGetGroups from "../../Hooks/UseGetGroups";
import useGetUser from "../../Hooks/UseGetUser";
import SignUpPaths from "../../Routers/SignUpRouter/Paths";

import DashboardSetup, {
  DashboardStatus as DashboardStatusObj,
  ExternalProps,
} from "./Dashboard";
export type DashboardStatus = DashboardStatusObj;
export type Props = ExternalProps;

export const useGetDashboardData = (): DashboardStatus => {
  const { sharedInvite, hasSeenIntro } = useSelector((state: State) => ({
    sharedInvite: state.timeSlots.sharedPartnerInvite,
    chosenCarDesign: state.timeSlots.choseCardDesign,
    hasSeenIntro: state.seenIntro,
  }));

  const groups = useGetGroups();
  let partnerAccepted = false;
  if (groups.data !== undefined) {
    partnerAccepted =
      groups.data.filter((g) => g.status === "groupactive").length > 0;
  }

  const user = useGetUser();
  let hasLinked = false;
  let hasVerified = false;
  let hasVerifiedIdentity = false;
  if (user !== null) {
    hasLinked = user.user.ach.ddaExists;
    hasVerified =
      user.user.ach.verified === null ? false : user.user.ach.verified;
    hasVerifiedIdentity = user.user.status.tag === "UserActive";
  }

  return {
    hasSeenIntro,
    hasVerifiedIdentity,
    hasInvitedPartner: sharedInvite === undefined ? false : true,
    partnerAccepted,
    haslinkedBank: hasLinked,
    hasVerifiedBank: hasVerified,
    userState: defaultTo(user?.user.status.tag, null),
    // chosenCardDesign: chosenCarDesign === undefined ? false : true,
    userCreatedOn:
      user === null ? null : new Date(user.user.timestamps.created),
    partnerLinked: defaultTo(user?.partner?.linked, false),
    partnerVerified: defaultTo(user?.partner?.verified, false),
    partnerName: defaultTo(user?.partner?.firstName, null),
  };
};

export default function DashboardHOC(props: Props): JSX.Element {
  const dispatch = useDispatch<AlfieDispatch>();
  const dashStatus = useGetDashboardData();

  if (
    dashStatus.userState === "UserKYCDelay" ||
    dashStatus.userState === "UserUpdatedKYCDelay" ||
    dashStatus.userState === "UserWaitingOnKYC"
  ) {
    return <Redirect to={SignUpPaths.kycManual} />;
  }

  return (
    <DashboardSetup
      {...dashStatus}
      setLoadedGift={() => {
        void Analytics.track("QuestScreen WelcomeGift Tapped");
        dispatch({ type: "setTimeslot", key: "loadedGift", value: new Date() });
      }}
      gotoAcceptCode={() => {
        void Analytics.track("QuestScreen Acceptinvite Tapped");
        props.gotoAcceptCode();
      }}
      gotoInviteCode={() => {
        void Analytics.track("QuestScreen InvitePartner Tapped");
        props.gotoInviteCode();
      }}
      gotoLinkBank={() => {
        void Analytics.track("QuestScreen LinkFS Tapped");
        props.gotoLinkBank();
      }}
      gotoVerifyBank={() => {
        void Analytics.track("QuestScreen VerifyFS Tapped");
        props.gotoVerifyBank();
      }}
      gotoPurhcase={() => {
        void Analytics.track("QuestScreen MakePurchase Tapped");
        props.gotoPurhcase();
      }}
      gotoIntro={props.gotoIntro}
      gotoSignup={props.gotoSignup}
    />
  );
}
