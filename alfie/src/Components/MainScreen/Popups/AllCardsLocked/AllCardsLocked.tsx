/* eslint-disable react-native/no-color-literals */
/* eslint-disable react-native/no-inline-styles */
import React, { useContext, useEffect, useState } from "react";
import { Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import styles from "../Styles";
import Button from "../../../Button/Button";
import ContactSupport from "../../../Button/ContactSupport";
import TextStyles from "../../../Styling/Text";
import { ThemeContext } from "../../../Theming/ThemeContext";

interface Props {
  primaryAction: () => void;
  trackSeen: () => void;
  trackIgnored: () => void;
}

const LinkFSPopUp = (props: Props): JSX.Element => {
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

  const tappedPrimary = () => {
    disableAgreeButton(true);
    showTheScreen(false);
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
              Card locked
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.subHeading,
                { color: theme.textColor },
              ]}
            >
              It looks like your card has been locked! No purchases will be
              authorized on your card, your partner's card will still work
              though. You can unlock your card on the settings screen.
            </Text>
          </View>
          <View style={styles.buttonSection}>
            <Button
              onPress={tappedPrimary}
              style="Primary"
              text="Good to know"
              inProgress={agreeDisabled}
            />
            <ContactSupport emailSubject="My card is locked" />
          </View>
        </View>
      </View>
    );
  }

  return <></>;
};

export default LinkFSPopUp;
