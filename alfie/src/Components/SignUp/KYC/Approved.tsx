import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../../Form/FormScreen";
import Button from "../../Button/Button";
import TextStyles from "../../Styling/Text";
import { ThemeContext } from "../../Theming/ThemeContext";

interface Props {
  gotoNext: () => void;
}

const KYCApproved = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      testID="KYCApproved FormScreen"
      title="Welcome"
      navigation={{ type: "none" }}
      buttons={
        <>
          <Button
            testID="KYCApproved Continue"
            text="Continue"
            style="Primary"
            onPress={props.gotoNext}
          />
        </>
      }
    >
      <View style={styles.frame}>
        <Text
          style={[TextStyles.fontWeight600, theme.textStyle, styles.heading]}
        >
          You're all good
        </Text>
        <Text style={[TextStyles.fontWeight400, theme.textStyle, styles.text]}>
          Welcome to Pay Tgthr! Your virtual Tgthr Card has been delivered to
          your phone.
        </Text>
      </View>
    </FormScreen>
  );
};

export default KYCApproved;

const styles = StyleSheet.create({
  frame: {
    alignItems: "center",
    justifyContent: "center",
  },
  heading: {
    fontSize: 21,
    marginBottom: 16,
    marginTop: 45,
  },
  text: {
    fontSize: 16,
    textAlign: "center",
  },
});
