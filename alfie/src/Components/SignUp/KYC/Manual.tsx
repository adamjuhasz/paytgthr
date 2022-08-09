import React, { useContext } from "react";
import { StyleSheet, Text, View } from "react-native";

import FormScreen from "../../Form/FormScreen";
import ContactSupportButton from "../../Button/ContactSupport";
import TextStyles from "../../Styling/Text";
import { ThemeContext } from "../../Theming/ThemeContext";

const KYCManual = (): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      title="Identity verification"
      navigation={{ type: "none" }}
      buttons={
        <>
          <ContactSupportButton
            emailSubject="I need to verify my identity"
            style="Primary"
          />
        </>
      }
    >
      <View style={styles.frame}>
        <Text
          style={[theme.textStyle, TextStyles.fontWeight600, styles.heading]}
        >
          Manual verification needed
        </Text>
        <Text style={[theme.textStyle, TextStyles.fontWeight400, styles.text]}>
          Please use the button below to send us a selfie holding your
          government ID
        </Text>
      </View>
    </FormScreen>
  );
};

export default KYCManual;

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
