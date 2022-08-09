import React, { useContext } from "react";
import { Text } from "react-native";

import FormStyles from "./Styling";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

const Label = (props: React.PropsWithChildren<unknown>): JSX.Element => {
  const theme = useContext(ThemeContext);

  return (
    <Text style={[theme.textStyle, TextStyles.fontWeight400, FormStyles.label]}>
      {props.children}
    </Text>
  );
};

export default Label;
