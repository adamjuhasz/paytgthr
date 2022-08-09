/* eslint-disable react-native/no-color-literals */
import React, { useContext, useState } from "react";
import {
  Platform,
  Pressable,
  StyleSheet,
  Text,
  TextStyle,
  View,
  ViewStyle,
} from "react-native";
import { LinearGradient } from "expo-linear-gradient";

import TextStyles from "../../Styling/Text";
import { grayBorder } from "../../Styling/Colors";
import { ThemeContext } from "../../Theming/ThemeContext";
import Checkmark from "./Checkmark";

export type StepType =
  | "Watch Intro"
  | "Verify Identity"
  | "Invite Partner"
  | "Link Bank"
  | "Wait on Partner"
  | "Make 1st Purchase";

export const stepToNumber = (step: StepType): number => {
  switch (step) {
    case "Watch Intro":
      return 1;
    case "Verify Identity":
      return 2;
    case "Invite Partner":
      return 3;
    case "Link Bank":
      return 4;
    case "Wait on Partner":
      return 5;
    case "Make 1st Purchase":
      return 6;
  }
};

export interface Props {
  stepNumber: StepType;
  stepTitle: string;
  stepState: "Completed" | "Active" | "Upcoming" | "WaitingOn";
  stepAction: () => void;
}

export default function Step(props: Props): JSX.Element {
  const theme = useContext(ThemeContext);
  const [tapping, setTapping] = useState(false);

  let borderStyle: ViewStyle = styles.borderUpcoming;
  let numberStyle: ViewStyle = styles.backgroundUpcoming;
  let backgroundStyle: ViewStyle = { backgroundColor: theme.backgroundColor };
  let numberText: TextStyle = { color: "#FFFFFF" };
  let textStyle: TextStyle = theme.textStyle;

  switch (props.stepState) {
    case "Completed":
      borderStyle = styles.borderCompleted;
      numberStyle = styles.backgroundCompleted;
      break;
    case "Active":
      numberStyle = { backgroundColor: "white" };
      numberText = { color: "#F7607A" };
      textStyle = { color: "white" };
      break;
    case "Upcoming":
      numberText = { color: theme.backgroundColor };
      if (theme.scheme === "light") {
        textStyle = { color: grayBorder };
        borderStyle = { borderColor: grayBorder };
        numberStyle = { backgroundColor: grayBorder };
      } else {
        textStyle = { color: "#95959A" };
        borderStyle = { borderColor: "#3E3E3E" };
        numberStyle = { backgroundColor: "#95959A" };
      }
      break;
    case "WaitingOn":
      borderStyle = { borderColor: "#F6D549" };
      numberStyle = { backgroundColor: "#F6D549" };
  }

  let Container = (p: React.PropsWithChildren<unknown>) => (
    <View style={[styles.box, borderStyle, backgroundStyle]}>{p.children}</View>
  );

  switch (props.stepState) {
    case "Upcoming":
    case "WaitingOn":
      break;

    case "Completed":
      Container = function ContainerGradient(
        p: React.PropsWithChildren<unknown>
      ) {
        const primaryColor = "#3AD79E";
        return (
          <LinearGradient
            style={[
              styles.box,
              styles.noBorder,
              { backgroundColor: primaryColor },
            ]}
            colors={["#3AD79E", "#36B2B9"]}
          >
            {p.children}
          </LinearGradient>
        );
      };
      break;

    case "Active":
      Container = function ContainerGradient(
        p: React.PropsWithChildren<unknown>
      ) {
        const primaryColor = "#F65E93";
        return (
          <LinearGradient
            style={[
              styles.box,
              styles.noBorder,
              { backgroundColor: primaryColor },
            ]}
            colors={["#F65E93", "#FD7776"]}
          >
            {p.children}
          </LinearGradient>
        );
      };
      break;
  }

  if (tapping === true) {
    backgroundStyle = { backgroundColor: theme.backgroundColor };
    textStyle = theme.textStyle;
    numberStyle = { backgroundColor: "#F7607A" };
    numberText = { color: "white" };
  }

  const showFiller = tapping || props.stepState === "Completed";

  const WhiteFiller = () => (
    <View style={[styles.boxBackground, backgroundStyle]} />
  );

  let number = (
    <Text style={[TextStyles.nexaHeavy, styles.stepNumber, numberText]}>
      {stepToNumber(props.stepNumber)}
    </Text>
  );
  if (props.stepState === "Completed") {
    number = <Checkmark />;
  }

  return (
    <View style={[styles.container]}>
      <Pressable
        onPressIn={() => setTapping(true)}
        onPressOut={() => setTapping(false)}
        onPress={props.stepAction}
        disabled={props.stepState !== "Active"}
      >
        <Container>
          {showFiller ? <WhiteFiller /> : <></>}
          <View style={[styles.numberBall, numberStyle]}>{number}</View>
          <View style={[styles.stepTitleContainer]}>
            <View style={[StyleSheet.absoluteFill, styles.stepTitle]}>
              <Text
                style={[
                  props.stepState === "Active"
                    ? TextStyles.fontWeight600
                    : TextStyles.fontWeight600,
                  styles.stepTitleText,
                  textStyle,
                ]}
              >
                {props.stepTitle}
              </Text>
            </View>
          </View>
        </Container>
      </Pressable>
    </View>
  );
}

const styles = StyleSheet.create({
  backgroundCompleted: {
    backgroundColor: "#37C89D",
  },
  backgroundUpcoming: {
    backgroundColor: grayBorder,
  },
  borderCompleted: {
    borderColor: "#159A72",
  },
  borderUpcoming: {
    borderColor: grayBorder,
  },
  box: {
    alignItems: "center",
    borderRadius: 10,
    borderWidth: 2,
    flexDirection: "row",
    height: 75,
    justifyContent: "space-between",
    paddingHorizontal: 20,
    position: "relative",
    width: "100%",
  },
  boxBackground: {
    borderRadius: 8,
    bottom: 2,
    left: 2,
    position: "absolute",
    right: 2,
    top: 2,
  },
  container: { width: "100%" },
  noBorder: { borderWidth: 0 },
  numberBall: {
    alignItems: "center",
    borderRadius: 9999,
    height: 40,
    justifyContent: "center",
    width: 40,
  },
  stepNumber: {
    fontSize: 20,
    height: Platform.OS === "ios" ? 18 : 22,
    marginLeft: Platform.OS === "ios" ? 1 : 0,
    marginTop: Platform.OS === "ios" ? 2 : -3,
  },
  stepTitle: { justifyContent: "center" },
  stepTitleContainer: {
    flexGrow: 1,
    height: "100%",
    marginLeft: 20,
    position: "relative",
  },
  stepTitleText: { fontSize: 15, justifyContent: "center" },
});
