/* eslint-disable react-native/no-color-literals */
/* eslint-disable react-native/no-inline-styles */
import React, { useEffect, useState } from "react";
import {
  Linking,
  StyleSheet,
  Text,
  TouchableOpacity,
  View,
} from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import Button from "../../../Button/Button";
import TextStyles from "../../../Styling/Text";

interface Props {
  submitAgreement: () => Promise<void>;
  trackSeen: () => void;
  trackIgnored: () => void;
}

const PrivacyConsents = (props: Props): JSX.Element => {
  const [trackSeen, markSeen] = useState(false);
  const [showScreen, showTheScreen] = useState(true);
  const [agreeDisabled, disableAgreeButton] = useState(false);
  const insets = useSafeAreaInsets();

  useEffect(() => {
    if (trackSeen === false) {
      markSeen(true);
      props.trackSeen();
    }
  }, [trackSeen, props]);

  const submitAgreement = async () => {
    disableAgreeButton(true);
    showTheScreen(false);
    await props.submitAgreement();
  };

  if (showScreen) {
    return (
      <View
        style={[
          styles.background,
          {
            shadowColor: "#DA525D",
            shadowOffset: {
              width: 0,
              height: 0,
            },
            shadowOpacity: 0.58,
            shadowRadius: 16.0,

            elevation: 24,
          },
        ]}
      >
        <View
          style={[
            styles.popup,
            // eslint-disable-next-line react-native/no-inline-styles
            { bottom: insets.bottom == 0 ? 20 : insets.bottom },
          ]}
        >
          <View style={styles.textSection}>
            <Text style={[TextStyles.nexaHeavy, styles.heading]}>
              We're making you some fresh new cards
            </Text>
            <Text style={[TextStyles.fontWeight400, styles.subHeading]}>
              Before we can issue you a new virtual Tgthr Card, you'll need to
              agree to the following agreements:
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.bulletpoint,
                styles.bulletpointText,
              ]}
            >
              •{" "}
              <TouchableOpacity
                onPress={() =>
                  Linking.openURL(
                    "https://paytgthr.com/legal/termsofservice.pdf"
                  )
                }
              >
                <Text
                  style={[TextStyles.fontWeight400, styles.bulletpointText]}
                >
                  Pay Tgthr Terms of Service
                </Text>
              </TouchableOpacity>
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.bulletpoint,
                styles.bulletpointText,
              ]}
            >
              •{" "}
              <TouchableOpacity
                onPress={() =>
                  Linking.openURL(
                    "https://paytgthr.com/legal/esignagrreement-2021-05-15.pdf"
                  )
                }
              >
                <Text
                  style={[TextStyles.fontWeight400, styles.bulletpointText]}
                >
                  E-SIGN Consent Agreement
                </Text>
              </TouchableOpacity>
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.bulletpoint,
                styles.bulletpointText,
              ]}
            >
              •{" "}
              <TouchableOpacity
                onPress={() =>
                  Linking.openURL(
                    "https://paytgthr.com/legal/ach-auth-2021-05-15.pdf"
                  )
                }
              >
                <Text
                  style={[TextStyles.fontWeight400, styles.bulletpointText]}
                >
                  ACH Authorization
                </Text>
              </TouchableOpacity>
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.bulletpoint,
                styles.bulletpointText,
              ]}
            >
              •{" "}
              <TouchableOpacity
                onPress={() =>
                  Linking.openURL(
                    "https://paytgthr.com/legal/cardholderagreement-2021-05-15.pdf"
                  )
                }
              >
                <Text
                  style={[TextStyles.fontWeight400, styles.bulletpointText]}
                >
                  Pay Tgthr Cardholder Agreement
                </Text>
              </TouchableOpacity>
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.bulletpoint,
                styles.bulletpointText,
              ]}
            >
              •{" "}
              <TouchableOpacity
                onPress={() =>
                  Linking.openURL(
                    "https://paytgthr.com/legal/privacypolicy.pdf"
                  )
                }
              >
                <Text
                  style={[TextStyles.fontWeight400, styles.bulletpointText]}
                >
                  Pay Tgthr Privacy Policy
                </Text>
              </TouchableOpacity>
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.bulletpoint,
                styles.bulletpointText,
              ]}
            >
              •{" "}
              <TouchableOpacity
                onPress={() =>
                  Linking.openURL(
                    "https://paytgthr.com/legal/privacypolicy-patriot.pdf"
                  )
                }
              >
                <Text
                  style={[TextStyles.fontWeight400, styles.bulletpointText]}
                >
                  Patriot Bank Privacy Policy
                </Text>
              </TouchableOpacity>
            </Text>
          </View>
          <View style={styles.buttonSection}>
            <Button
              onPress={submitAgreement}
              style="Primary"
              text="I accept"
              inProgress={agreeDisabled}
            />
            <Text style={[TextStyles.fontWeight400, styles.footer]}>
              Your current card no longer works
            </Text>
          </View>
        </View>
      </View>
    );
  }

  return <></>;
};

export default PrivacyConsents;

const dimmedBlack = "rgba(0,0,0,0.7)";
const crispWhite = "#FFFFFF";
const nightBlack = "#000000";

const styles = StyleSheet.create({
  background: {
    backgroundColor: dimmedBlack,
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
  bulletpoint: { marginLeft: 10 },
  bulletpointText: { color: nightBlack, fontSize: 13 },
  buttonSection: { marginBottom: -10 },
  footer: {
    color: nightBlack,
    fontSize: 13,
    marginTop: 0,
    textAlign: "center",
  },
  heading: { color: nightBlack, fontSize: 28, marginBottom: 10 },
  popup: {
    backgroundColor: crispWhite,
    borderRadius: 20,
    left: 0,
    marginHorizontal: 20,
    padding: 20,
    position: "absolute",
    right: 0,
  },
  subHeading: {
    color: nightBlack,
    fontSize: 16,
    marginBottom: 5,
  },
  textSection: { flex: 1, marginBottom: 30 },
});
