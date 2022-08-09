import React, { useContext, useState } from "react";
import { Linking, StyleSheet, Text, TouchableOpacity } from "react-native";

import FormScreen from "../../Form/FormScreen";
import Button from "../../Button/Button";
import ProgressButton from "../../Button/ProgressButton";
import TextStyles from "../../Styling/Text";
import PopUp from "../PopUp/PopUp";
import { ThemeContext } from "../../Theming/ThemeContext";
import { TrackEvent } from "../../../PlatformSpecific/SegmentAnalyticsType";

interface LegalConsentProps {
  inProgress: boolean;
  submitConsent: () => void;
  wontAccept: () => void;
  showHelp?: boolean;
  trackEvent: TrackEvent;
  screenIndex: number;
  screenCount: number;
  goBack: () => void;
}

export const LegalConsent = (props: LegalConsentProps): JSX.Element => {
  const theme = useContext(ThemeContext);
  const [showHelp, setShow] = useState<boolean>(false);

  return (
    <>
      <FormScreen
        testID="LegalConsent FormScreen"
        title="Legal"
        navigation={{ type: "action", action: props.goBack }}
        buttons={
          <>
            <Button
              text="Why do you need this?"
              onPress={() => {
                setShow(true);
                void props.trackEvent("User Signup PopUp Opened", {
                  screen: "LegalConsent",
                });
              }}
              style="Secondary"
            />
            <Button
              testID="LegalConsent WontAccept"
              text="I don't agree"
              onPress={props.wontAccept}
              style="Secondary"
              inProgress={props.inProgress}
            />
            <ProgressButton
              testID="LegalConsent Continue"
              text="Agree"
              onPress={props.submitConsent}
              style="Primary"
              inProgress={props.inProgress}
              index={props.screenIndex}
              count={props.screenCount}
            />
          </>
        }
      >
        <Text style={[TextStyles.fontWeight400, theme.textStyle, styles.text]}>
          By clicking Agree & Continue below, you agree to
        </Text>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL("https://paytgthr.com/legal/termsofservice.pdf")
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • Pay Tgthr Terms of Service
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL(
              "https://paytgthr.com/legal/esignagrreement-2021-05-15.pdf"
            )
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • E-SIGN Consent Agreement
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL(
              "https://paytgthr.com/legal/cardholderagreement-2021-05-15.pdf"
            )
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • The Pay Tgthr Cardholder Agreement
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL(
              "https://paytgthr.com/legal/ach-auth-2021-05-15.pdf"
            )
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • ACH Authorization
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL("https://paytgthr.com/legal/privacypolicy.pdf")
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • Pay Tgthr’s Privacy Policy
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL(
              "https://paytgthr.com/legal/privacypolicy-patriot.pdf"
            )
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • Patriot Bank’s Privacy Policy
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() =>
            Linking.openURL("https://www.dwolla.com/legal/privacy/")
          }
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • Dwolla Privacy Policy
          </Text>
        </TouchableOpacity>
        <TouchableOpacity
          onPress={() => Linking.openURL("https://www.dwolla.com/legal/tos/")}
        >
          <Text
            style={[TextStyles.fontWeight400, theme.textStyle, styles.bullets]}
          >
            • Dwolla Terms of Service
          </Text>
        </TouchableOpacity>
      </FormScreen>
      <PopUp
        headingText="Lawyers say what?"
        bodyText={
          "To help the government fight the funding of terrorism and money laundering activities, federal law requires all financial institutions to obtain, verify, and record information that identifies each person who opens an account.\n\nWhat this means for you: When you open an account, we will ask for your name, address, date of birth, and other information that will allow us to identify you. We may also ask to see your driver’s license or other identifying documents."
        }
        buttonText="Got it"
        enabled={showHelp}
        setEnabled={setShow}
      />
    </>
  );
};
LegalConsent.displayName = "LegalConsent";

export default LegalConsent;

const styles = StyleSheet.create({
  bullets: {
    fontSize: 16,
    paddingHorizontal: 25,
  },
  text: {
    fontSize: 20,
    marginBottom: 5,
    paddingHorizontal: 10,
  },
});
