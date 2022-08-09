import React, { useContext } from "react";
import {
  Platform,
  TextInput as RNTextInput,
  TextInputProps,
} from "react-native";

import FormStyles from "./Styling";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

const TextInput = React.forwardRef<RNTextInput, TextInputProps>(
  (props, ref): JSX.Element => {
    const theme = useContext(ThemeContext);

    const ligtherColor = `${theme.textColor}${
      theme.scheme === "light" ? 24 : 30
    }`;

    return (
      <RNTextInput
        style={[
          theme.textStyle,
          TextStyles.fontWeight400,
          FormStyles.inputText,
          { borderBottomColor: ligtherColor },
        ]}
        selectionColor={theme.textColor}
        placeholderTextColor={ligtherColor}
        returnKeyType={"next"}
        blurOnSubmit={Platform.OS === "android" ? false : true} // android has weird keyboard flashing
        ref={ref}
        {...props}
      />
    );
  }
);
TextInput.displayName = "TextInput";

export default TextInput;
