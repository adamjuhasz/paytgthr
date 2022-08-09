/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import ActivateCard from "./ActivateCard";

export default {
  title: "Screens/ActivateCard",
  component: ActivateCard,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof ActivateCard>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <ActivateCard {...args} />
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

BaseState.args = {
  inProgress: false,
};
