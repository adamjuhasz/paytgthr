import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../../Form/FormScreen";
import ContactSupportButton from "../../Button/ContactSupport";
import TextStyles from "../../Styling/Text";
import { ThemeContext } from "../../Theming/ThemeContext";

const KYCFailed = (): JSX.Element => {
  const theme = useContext(ThemeContext);
  return (
    <FormScreen
      title="Identity verification"
      navigation={{ type: "none" }}
      buttons={
        <>
          <ContactSupportButton emailSubject="" style="Primary" />
        </>
      }
    >
      <View style={styles.frame}>
        <Text
          style={[theme.textStyle, TextStyles.fontWeight600, styles.heading]}
        >
          Houston, we have a problem
        </Text>
        <Text style={[theme.textStyle, TextStyles.fontWeight400, styles.text]}>
          Oh no, at this time we're not able to let you open an account
        </Text>
      </View>
    </FormScreen>
  );
};

export default KYCFailed;

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
