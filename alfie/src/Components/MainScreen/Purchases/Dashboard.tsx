/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { View } from "react-native";

import PersonalLimit from "./PersonalLimit";

interface Props {
  demoMode: boolean;
  canSpend: number;
  maxSpend: number;
}

export default function Cards(props: Props): JSX.Element {
  return (
    <View
      style={[
        { marginTop: -20, paddingHorizontal: 28 },
        { marginBottom: props.demoMode ? 25 : 20 },
      ]}
    >
      <PersonalLimit canSpend={props.canSpend} maxSpend={props.maxSpend} />
    </View>
  );
}
