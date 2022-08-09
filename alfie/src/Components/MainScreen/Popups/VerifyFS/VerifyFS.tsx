/* eslint-disable react-native/no-color-literals */
/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useEffect, useState } from "react";
import { Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import styles from "../Styles";
import Button from "../../../Button/Button";
import TextStyles from "../../../Styling/Text";
import { ThemeContext } from "../../../Theming/ThemeContext";

interface Props {
  primaryAction: () => void;
  trackSeen: () => void;
  trackIgnored: () => void;
}

const VerifyFSPopup = (props: Props): JSX.Element => {
  const [trackSeen, markSeen] = useState(false);
  const [showScreen, showTheScreen] = useState(true);
  const [agreeDisabled, disableAgreeButton] = useState(false);
  const insets = useSafeAreaInsets();
  const theme = useContext(ThemeContext);

  useEffect(() => {
    if (trackSeen === false) {
      markSeen(true);
      props.trackSeen();
    }
  }, [trackSeen, props]);

  const submitAgreement = () => {
    disableAgreeButton(true);
    showTheScreen(false);
    props.primaryAction();
  };

  if (showScreen) {
    return (
      <View
        style={[
          styles.background,
          {
            shadowColor: theme.scheme === "light" ? "#000000" : "#DA525D",
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
            { backgroundColor: theme.backgroundColor },
          ]}
        >
          <View style={styles.textSection}>
            <Text
              style={[
                TextStyles.nexaHeavy,
                styles.heading,
                { color: theme.textColor },
              ]}
            >
              Need to verify your bank account
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.subHeading,
                { color: theme.textColor },
              ]}
            >
              Before you can use your new card, you'll need to verify the bank
              account you linked by verifying the amount of the
              micro-withdrawal.
            </Text>
          </View>
          <View style={styles.buttonSection}>
            <Button
              onPress={submitAgreement}
              style="Primary"
              text="Verify bank account"
              inProgress={agreeDisabled}
            />
            <Button
              onPress={() => {
                props.trackIgnored();
                showTheScreen(false);
              }}
              style="Secondary"
              text="I'll do it later"
              inProgress={agreeDisabled}
            />
          </View>
        </View>
      </View>
    );
  }

  return <></>;
};

export default VerifyFSPopup;
