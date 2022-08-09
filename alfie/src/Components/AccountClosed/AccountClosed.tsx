import React, { useContext } from "react";
import { Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import TextStyles from "../Styling/Text";
import ContactSupport from "../Button/ContactSupport";
import { ThemeContext } from "../Theming/ThemeContext";

export const UserClosed = (): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <FormScreen
      navigation={{ type: "none" }}
      buttons={<ContactSupport emailSubject="Hi there!" />}
    >
      <Text
        style={[TextStyles.fontWeight400, TextStyles.centered, theme.textStyle]}
      >
        Your account has been closed
      </Text>
    </FormScreen>
  );
};

export default UserClosed;
