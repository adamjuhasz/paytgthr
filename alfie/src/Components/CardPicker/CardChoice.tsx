import React, { useState } from "react";
import { Pressable, StyleSheet, View } from "react-native";

import { grayBorder, pinkColor } from "../Styling/Colors";

interface Props<T> {
  key: T;
  selected: boolean;
  onPress: (key: T) => void;
}

export default function CardChoice<T>(
  props: React.PropsWithChildren<Props<T>>
): JSX.Element {
  const [pressedIn, setPressed] = useState(false);

  const highlighted = props.selected || pressedIn;

  return (
    <Pressable
      onPress={() => props.onPress(props.key)}
      onPressIn={() => setPressed(true)}
      onPressOut={() => setPressed(false)}
    >
      <View
        style={[
          styles.box,
          styles.border,
          highlighted ? styles.selectedBorder : undefined,
        ]}
      >
        {props.children}
      </View>
    </Pressable>
  );
}

const styles = StyleSheet.create({
  border: {
    borderColor: grayBorder,
    borderRadius: 10,
    borderWidth: 1,
  },
  box: {
    height: 75,
    width: "100%",
  },
  selectedBorder: {
    borderColor: pinkColor,
    borderWidth: 2,
  },
});
