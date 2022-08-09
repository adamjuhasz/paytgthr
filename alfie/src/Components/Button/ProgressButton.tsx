import React from "react";
import { StyleSheet, View } from "react-native";

import Dots, { Props as DotProps } from "../DotProgress/DotProgress";
import Button, { Props as ButtonProps } from "./Button";

type Props = DotProps & ButtonProps;

const ProgressButton = (props: Props): JSX.Element => {
  return (
    <View style={[styles.container]}>
      <Dots {...props} />
      <View style={[styles.buttonRight]}>
        <Button {...props} />
      </View>
    </View>
  );
};

export default ProgressButton;

const styles = StyleSheet.create({
  buttonRight: {
    width: 153,
  },
  container: {
    alignItems: "center",
    flexDirection: "row",
    justifyContent: "space-between",
    width: "100%",
  },
});
