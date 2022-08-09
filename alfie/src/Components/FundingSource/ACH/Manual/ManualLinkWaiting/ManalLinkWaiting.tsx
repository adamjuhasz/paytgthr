import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../../../../Form/FormScreen";
import Button from "../../../../Button/Button";
import TextStyles from "../../../../Styling/Text";
import { ThemeContext } from "../../../../Theming/ThemeContext";

interface Props {
  nextScreen: () => void;
}

export const ManualLinkWaiting = (props: Props): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      testID="ManualLinkWaiting FormScreen"
      navigation={{ type: "none" }}
      buttons={
        <>
          <Button
            testID="ManualLinkWaiting Continue"
            text="Continue"
            style="Primary"
            onPress={props.nextScreen}
          />
        </>
      }
    >
      <View style={styles.frame}>
        <Text
          style={[TextStyles.fontWeight600, styles.heading, theme.textStyle]}
        >
          Almost there
        </Text>
        <Text style={[TextStyles.fontWeight400, styles.text, theme.textStyle]}>
          It will take 2-3 business days for a small (less than $2.50) debit or
          credit to show up in your account. When it does, enter the amount of
          the withdrawal on the next screen so we know the account belongs to
          you.
        </Text>
      </View>
    </FormScreen>
  );
};

export default ManualLinkWaiting;

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
