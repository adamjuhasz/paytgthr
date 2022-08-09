/* eslint-disable react-native/no-color-literals */
import React, { useContext, useEffect, useState } from "react";
import {
  FlatList,
  Pressable,
  StyleSheet,
  Text,
  View,
  useWindowDimensions,
} from "react-native";
import { defaultTo, padStart } from "lodash";
import { useApplicationInstallTime } from "@use-expo/application";

import Step, { Props as StepProps, StepType } from "./Step";
import TextStyles from "../../Styling/Text";
import Confetti from "../../Confetti/Confetti";
import { ThemeContext } from "../../Theming/ThemeContext";
import { UserStates } from "../../../Types/UserStateTypes";

export interface ExternalProps {
  gotoIntro: () => void;
  gotoSignup: () => void;
  gotoInviteCode: () => void;
  gotoAcceptCode: () => void;
  gotoLinkBank: () => void;
  gotoVerifyBank: () => void;
  gotoPurhcase: () => void;
}

export interface DashboardStatus {
  hasSeenIntro: boolean;
  hasVerifiedIdentity: boolean;
  hasInvitedPartner: boolean;
  partnerAccepted: boolean;
  haslinkedBank: boolean;
  hasVerifiedBank: boolean;
  userState: UserStates | null;
  userCreatedOn: Date | null;
  partnerLinked: boolean;
  partnerVerified: boolean;
  partnerName: null | string;
}

function calcTimeLeft(createdOn: Date) {
  if (createdOn == null) {
    return null;
  }
  const diff = Date.now() - createdOn.valueOf();
  if (diff > 86400000) {
    return null;
  }
  const hours = 24 - diff / 1000 / 3600;
  const minutes = (hours % 1) * 60;
  const seconds = (minutes % 1) * 60;

  return {
    hours: Math.floor(hours),
    minutes: Math.floor(minutes),
    seconds: Math.floor(seconds),
  };
}

const appOpenTime = new Date();
function correctNonUserTime(installTime: Date | undefined) {
  let nonUserTime = defaultTo(installTime, appOpenTime);
  if (calcTimeLeft(nonUserTime) === null) {
    nonUserTime = appOpenTime;
  }
  return nonUserTime;
}

export interface Props {
  setLoadedGift: () => void;
}

export default function DashboardSetup(
  props: Props & ExternalProps & DashboardStatus
): JSX.Element {
  const [showConf, setConfetti] = useState(false);
  const theme = useContext(ThemeContext);
  const { width } = useWindowDimensions();
  const [installTime] = useApplicationInstallTime({ get: true });
  const [timeLeft, setTimeLeft] = useState<null | {
    hours: number;
    minutes: number;
    seconds: number;
  }>(calcTimeLeft(correctNonUserTime(installTime)));

  useEffect(() => {
    if (props.userCreatedOn === null || props.userCreatedOn === undefined) {
      setTimeLeft(calcTimeLeft(correctNonUserTime(installTime)));
    } else {
      const diff = Date.now() - props.userCreatedOn.valueOf();

      if (diff > 86400000) {
        setTimeLeft(null);
        return;
      }

      setTimeLeft(calcTimeLeft(props.userCreatedOn));
    }

    const interval = setInterval(() => {
      const nonUserTime = correctNonUserTime(installTime);
      const usingTime = defaultTo(props.userCreatedOn, nonUserTime);
      setTimeLeft(calcTimeLeft(usingTime));
    }, 1000);

    return () => {
      clearInterval(interval);
    };
  }, [props.userCreatedOn, installTime]);

  let currentStep: StepType = "Watch Intro";
  if (props.hasSeenIntro) {
    currentStep = "Verify Identity";
    if (props.hasVerifiedIdentity) {
      currentStep = "Invite Partner";
      if (props.hasInvitedPartner || props.partnerAccepted) {
        currentStep = "Link Bank";
        if (props.hasVerifiedBank) {
          currentStep = "Wait on Partner";
          if (props.partnerAccepted && props.partnerVerified) {
            currentStep = "Make 1st Purchase";
          }
        }
      }
    }
  }

  const steps: StepProps[][] = [
    [
      {
        stepNumber: "Watch Intro",
        stepState: props.hasSeenIntro
          ? "Completed"
          : currentStep !== "Watch Intro"
          ? "Upcoming"
          : "Active",
        stepTitle: props.hasSeenIntro
          ? "Learn more about Pay Tgthr"
          : "Tap here to learn more about Pay Tgthr",
        stepAction: props.gotoIntro,
      },
    ],
    [
      {
        stepNumber: "Verify Identity",
        stepState: props.hasVerifiedIdentity
          ? "Completed"
          : currentStep !== "Verify Identity"
          ? "Upcoming"
          : "Active",
        stepTitle: props.hasVerifiedIdentity
          ? "Identity verified"
          : "Tap here to create an account",
        stepAction: props.gotoSignup,
      },
    ],
    props.partnerAccepted
      ? [
          {
            stepNumber: "Invite Partner",
            stepState: "Completed",
            stepTitle: `Partnered up${
              props.partnerName === null ? "" : ` with ${props.partnerName}`
            }`,
            stepAction: props.gotoAcceptCode,
          },
        ]
      : [
          {
            stepNumber: "Invite Partner",
            stepState:
              props.hasInvitedPartner || props.partnerAccepted
                ? "Completed"
                : currentStep !== "Invite Partner"
                ? "Upcoming"
                : "Active",
            stepTitle:
              props.hasInvitedPartner || props.partnerAccepted
                ? "Invite shared with partner"
                : "Tap here to share your code and invite your partner",
            stepAction: props.gotoInviteCode,
          },
          {
            stepNumber: "Invite Partner",
            stepState: props.partnerAccepted
              ? "Completed"
              : currentStep === "Watch Intro" ||
                currentStep === "Verify Identity"
              ? "Upcoming"
              : "Active",
            stepTitle: props.partnerAccepted
              ? "Partnered up"
              : "Tap here to enter your partner's invite code",
            stepAction: props.gotoAcceptCode,
          },
        ],
    props.hasVerifiedBank
      ? [
          {
            stepNumber: "Link Bank",
            stepState: "Completed",
            stepTitle: "Verified bank account",
            stepAction: props.gotoVerifyBank,
          },
        ]
      : [
          {
            stepNumber: "Link Bank",
            stepState:
              props.haslinkedBank || props.hasVerifiedBank
                ? "Completed"
                : currentStep !== "Link Bank"
                ? "Upcoming"
                : "Active",
            stepTitle: props.haslinkedBank
              ? "Linked bank account"
              : "Tap here to link your bank account",
            stepAction: props.gotoLinkBank,
          },
          {
            stepNumber: "Link Bank",
            stepState: props.hasVerifiedBank
              ? "Completed"
              : props.haslinkedBank === true
              ? "Active"
              : "Upcoming",
            stepTitle: props.hasVerifiedBank
              ? "Verified bank account"
              : "Tap here to verify your bank account",
            stepAction: props.gotoVerifyBank,
          },
        ],
    props.partnerVerified
      ? [
          {
            stepNumber: "Wait on Partner",
            stepState: "Completed",
            stepTitle: "Partner's bank account linked",
            stepAction: () => undefined,
          },
        ]
      : props.partnerAccepted
      ? [
          {
            stepNumber: "Wait on Partner",
            stepState: "WaitingOn",
            stepTitle: `Waiting on ${defaultTo(
              props.partnerName,
              "your partner"
            )} to link their bank account`,
            stepAction: () => undefined,
          },
        ]
      : [
          {
            stepNumber: "Wait on Partner",
            stepState: props.partnerAccepted
              ? "Completed"
              : currentStep === "Wait on Partner"
              ? "WaitingOn"
              : "Upcoming",
            stepTitle: props.partnerAccepted
              ? "Partnered up"
              : "Waiting on your partner to accept your invite",
            stepAction: () => undefined,
          },
          {
            stepNumber: "Wait on Partner",
            stepState: props.partnerVerified
              ? "Completed"
              : currentStep === "Wait on Partner"
              ? "WaitingOn"
              : "Upcoming",
            stepTitle: props.partnerVerified
              ? "Partner's bank account linked"
              : `Waiting on ${defaultTo(
                  props.partnerName,
                  "your partner"
                )} to link their bank account`,
            stepAction: () => undefined,
          },
        ],
    [
      {
        stepNumber: "Make 1st Purchase",
        stepState: currentStep === "Make 1st Purchase" ? "Active" : "Upcoming",
        stepTitle:
          "Use your Tgthr Card (FYI, your current spending limit is $150)",
        stepAction: () => {
          props.gotoPurhcase();
        },
      },
    ],
  ];

  let timer: JSX.Element =
    timeLeft !== null ? (
      <>
        <Text
          style={[
            TextStyles.fontWeight400,
            theme.textStyle,
            styles.listHeaderText,
            styles.countdownText,
          ]}
        >
          Help your partner sign up within 24 hours to get 10% cash back on your
          first purchase
        </Text>
        <Text
          style={[
            TextStyles.fontWeight600,
            theme.textStyle,
            styles.listHeaderText,
            styles.countdownExplainer,
          ]}
        >
          Time left
        </Text>
        <View style={[styles.countdownBoxes]}>
          <View style={[styles.contdownContainer]}>
            <Text style={[TextStyles.fontWeight600, styles.countdownBoxText]}>
              {padStart(timeLeft.hours.toFixed(0), 2, "0")}
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                theme.textStyle,
                styles.countdownDescText,
              ]}
            >
              hours
            </Text>
          </View>
          <View style={[styles.contdownContainer]}>
            <Text style={[TextStyles.fontWeight600, styles.countdownBoxText]}>
              {padStart(timeLeft.minutes.toFixed(0), 2, "0")}
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                theme.textStyle,
                styles.countdownDescText,
              ]}
            >
              minutes
            </Text>
          </View>
          <View style={[styles.contdownContainer]}>
            <Text style={[TextStyles.fontWeight600, styles.countdownBoxText]}>
              {padStart(timeLeft.seconds.toFixed(0), 2, "0")}
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                theme.textStyle,
                styles.countdownDescText,
              ]}
            >
              seconds
            </Text>
          </View>
        </View>
      </>
    ) : (
      <></>
    );

  if (props.partnerAccepted) {
    timer = <></>;
  }

  return (
    <View style={[StyleSheet.absoluteFill, styles.container]}>
      <FlatList
        alwaysBounceVertical={false}
        style={[styles.list]}
        data={steps}
        renderItem={({ item }) => (
          <FlatList
            style={[styles.horizontalList]}
            alwaysBounceHorizontal={false}
            horizontal
            pagingEnabled
            snapToAlignment="start"
            snapToInterval={width - 14 * 3}
            decelerationRate="fast"
            showsHorizontalScrollIndicator={false}
            initialNumToRender={2}
            getItemLayout={(_data, index) => ({
              length: width,
              offset: width * index - 14 * 3,
              index,
            })}
            data={item}
            ListHeaderComponent={() => (
              <View style={[styles.horizontalListHeaderFooter]} />
            )}
            ListFooterComponent={() => (
              <View style={[styles.horizontalListHeaderFooter]} />
            )}
            ItemSeparatorComponent={() => (
              <View style={[styles.horizontalListSeperator]} />
            )}
            initialScrollIndex={
              item.length === 1 ? 0 : item[0].stepState === "Completed" ? 1 : 0
            }
            renderItem={({ item: step }) => (
              <View style={[{ width: width - 28 * 2 }]}>
                <Step {...step} />
              </View>
            )}
            keyExtractor={(s) => `${s.stepNumber}-${s.stepTitle}`}
          />
        )}
        keyExtractor={(ses) =>
          ses.map((s) => `${s.stepNumber}-${s.stepTitle}`).join()
        }
        ItemSeparatorComponent={() => <View style={[styles.itemSeperator]} />}
        ListHeaderComponent={() => (
          <Pressable
            style={[styles.listHeader]}
            onPress={() => {
              setConfetti(true);
            }}
          >
            <Text
              style={[
                TextStyles.fontWeight600,
                theme.textStyle,
                styles.listHeaderText,
              ]}
            >
              Let's get set up
            </Text>
            {timer}
          </Pressable>
        )}
        ListFooterComponent={() => <View style={[styles.footer]} />}
      />
      {showConf ? <Confetti startOnLoad untilStopped /> : <></>}
    </View>
  );
}

const styles = StyleSheet.create({
  container: { marginTop: 32 },
  contdownContainer: {
    alignItems: "center",
    backgroundColor: "#F7B030",
    borderRadius: 10,
    height: 70,
    justifyContent: "center",
    marginHorizontal: 5,
    width: 60,
  },
  countdownBoxText: { fontSize: 20 },
  countdownBoxes: {
    flexDirection: "row",
    justifyContent: "center",
    width: "100%",
  },
  countdownDescText: {
    bottom: -20,
    left: 0,
    position: "absolute",
    right: 0,
    textAlign: "center",
  },
  countdownExplainer: { fontSize: 16, marginTop: 10, paddingHorizontal: 10 },
  countdownText: { fontSize: 16, marginTop: 5, paddingHorizontal: 10 },
  footer: { height: 29, width: "100%" },
  horizontalList: {
    width: "100%",
  },
  horizontalListHeaderFooter: { width: 28 },
  horizontalListSeperator: { width: 14 },
  itemSeperator: { height: 29, width: "100%" },
  list: { height: "100%", width: "100%" },
  listHeader: { marginVertical: 38 },
  listHeaderText: { fontSize: 28, textAlign: "center" },
});
