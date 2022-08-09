/* eslint-disable react-native/no-inline-styles */
import React, { useEffect } from "react";
import { Keyboard, StyleSheet, Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import Button from "../../Button/Button";
import TextStyles from "../../Styling/Text";
import { blackColor, whiteColor } from "../../Styling/Colors";
import { useScreenSize } from "../../Styling/PhoneSizes";

interface Props {
  headingText: string;
  bodyText: string;
  buttonText: string;
  enabled: boolean;
  setEnabled: (state: boolean) => void;
}

const PopUp = (props: Props): JSX.Element => {
  const insets = useSafeAreaInsets();
  const { narrowScreen } = useScreenSize();
  useEffect(() => {
    if (props.enabled) {
      Keyboard.dismiss();
    }
  }, [props.enabled]);

  if (props.enabled) {
    return (
      <View style={styles.background}>
        <View
          style={[
            styles.popup,
            { bottom: insets.bottom == 0 ? 20 : insets.bottom },
          ]}
        >
          <View style={styles.textSection}>
            <Text style={[TextStyles.nexaHeavy, styles.heading]}>
              {props.headingText}
            </Text>
            <Text
              style={[
                TextStyles.fontWeight400,
                styles.subHeading,
                { fontSize: narrowScreen ? 13 : 16 },
              ]}
            >
              {props.bodyText}
            </Text>
          </View>
          <View style={styles.buttonSection}>
            <Button
              onPress={() => {
                props.setEnabled(false);
              }}
              style="Primary"
              text={props.buttonText}
              styles={styles.button}
            />
          </View>
        </View>
      </View>
    );
  }

  return <></>;
};

export default PopUp;

const styles = StyleSheet.create({
  background: {
    backgroundColor: `${blackColor}B3`,
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
  button: { backgroundColor: blackColor },
  buttonSection: { marginBottom: -10 },
  heading: { color: blackColor, fontSize: 28, marginBottom: 10 },
  popup: {
    backgroundColor: whiteColor,
    borderRadius: 20,
    left: 0,
    marginHorizontal: 20,
    padding: 20,
    position: "absolute",
    right: 0,
  },
  subHeading: {
    color: blackColor,
    marginBottom: 5,
  },
  textSection: { flex: 1, marginBottom: 30 },
});
