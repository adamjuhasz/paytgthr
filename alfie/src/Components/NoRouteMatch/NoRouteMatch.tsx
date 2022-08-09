import React, { useContext } from "react";
import { Text } from "react-native";

import FormScreen from "../Form/FormScreen";
import Button from "../Button/Button";
import TextStyles from "../Styling/Text";
import { ThemeContext } from "../Theming/ThemeContext";

interface Props {
  action: () => void;
}

const NoRouteMatch = (props: React.PropsWithChildren<Props>): JSX.Element => {
  const theme = useContext(ThemeContext);
  return (
    <FormScreen
      title="Error"
      navigation={{ type: "none" }}
      buttons={
        <Button text="Fix glitch" style="Primary" onPress={props.action} />
      }
    >
      {props.children !== undefined ? (
        props.children
      ) : (
        <Text style={[TextStyles.fontWeight400, theme.textStyle]}>
          Oh no! Something went wrong in the matrix, let's try this again...
        </Text>
      )}
    </FormScreen>
  );
};

export default NoRouteMatch;
