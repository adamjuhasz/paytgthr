import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../../Form/FormScreen";
import ContactSupportButton from "../../Button/ContactSupport";
import TextStyles from "../../Styling/Text";
import { ThemeContext } from "../../Theming/ThemeContext";

const KYCDelay = (): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      title="Identity verification"
      navigation={{ type: "none" }}
      buttons={
        <>
          <ContactSupportButton
            emailSubject="What's the holdup with my verification?"
            style="Primary"
          />
        </>
      }
    >
      <View style={styles.frame}>
        <Text
          style={[TextStyles.fontWeight600, theme.textStyle, styles.heading]}
        >
          Sorry for the delay!
        </Text>
        <Text style={[TextStyles.fontWeight400, theme.textStyle, styles.text]}>
          We're double checking some information to verify you identity, this
          might take 1 to 2 days.
        </Text>
        <Text
          style={[
            TextStyles.fontWeight600,
            theme.textStyle,
            styles.text,
            styles.topMargin,
          ]}
        >
          Look out for an email from us in the next 5 minutes with next steps
        </Text>
      </View>
    </FormScreen>
  );
};

export default KYCDelay;

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
  topMargin: {
    marginTop: 16,
  },
});
