import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import FormScreen from "../../Form/FormScreen";
import TextStyles from "../../Styling/Text";
import Button from "../../Button/Button";
import { ThemeContext } from "../../Theming/ThemeContext";
import { blackColor, whiteColor } from "../../Styling/Colors";

interface Props {
  gotoNext: () => void;
  gotoSurvey: () => void;
}

const Survey = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <>
      <FormScreen
        title="Signup complete"
        navigation={{ type: "none" }}
        buttons={<></>}
      >
        <View style={styles.frame}>
          <Text
            style={[theme.textStyle, TextStyles.fontWeight600, styles.heading]}
          >
            All Done
          </Text>
          <Text
            style={[theme.textStyle, TextStyles.fontWeight400, styles.text]}
          >
            Thanks for signing up for a Tgthr Card
          </Text>
        </View>
      </FormScreen>
      <PopUp {...props} />
    </>
  );
};

const PopUp = (props: Props): JSX.Element => {
  const insets = useSafeAreaInsets();

  return (
    <View style={popupStyles.background}>
      <View
        style={[
          popupStyles.popup,
          // eslint-disable-next-line react-native/no-inline-styles
          { bottom: insets.bottom == 0 ? 20 : insets.bottom },
        ]}
      >
        <View style={popupStyles.textSection}>
          <Text style={[TextStyles.nexaHeavy, popupStyles.heading]}>
            Help us get better tgthr
          </Text>
          <Text style={[TextStyles.fontWeight400, popupStyles.subHeading]}>
            Please take 30 seconds to complete this short survey to help us get
            to you know you better!
          </Text>
          <Text style={[TextStyles.fontWeight400, popupStyles.notes]}>
            This info isn't used to verify your identity and doesn't affect your
            personal limit
          </Text>
        </View>
        <View style={popupStyles.buttonSection}>
          <Button
            onPress={() => {
              props.gotoNext();
            }}
            style="Secondary"
            text="Decline survey"
          />
          <Button
            onPress={() => {
              props.gotoSurvey();
            }}
            style="Primary"
            text="Take survey"
          />
        </View>
      </View>
    </View>
  );
};

export default Survey;

const styles = StyleSheet.create({
  frame: {
    alignItems: "center",
    justifyContent: "center",
  },
  heading: {
    fontSize: 21,
    marginBottom: 16,
  },
  text: {
    fontSize: 16,
    textAlign: "center",
  },
});

const dimmedBlack = "rgba(0,0,0,0.7)";

const popupStyles = StyleSheet.create({
  background: {
    backgroundColor: dimmedBlack,
    bottom: 0,
    left: 0,
    position: "absolute",
    right: 0,
    top: 0,
  },
  buttonSection: { marginBottom: -10 },
  heading: { color: blackColor, fontSize: 28, marginBottom: 10 },
  notes: {
    color: blackColor,
    fontSize: 13,
    marginBottom: 5,
  },
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
    fontSize: 16,
    marginBottom: 5,
  },
  textSection: { flex: 1, marginBottom: 30 },
});
