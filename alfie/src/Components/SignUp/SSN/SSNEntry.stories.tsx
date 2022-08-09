/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import SSNEntry from "./SSNEntry";

export default {
  title: "Screens/SignUp/SSNEntry",
  component: SSNEntry,
  argTypes: {
    submit: { action: "submit" },
    trackEvent: { action: "trackEvent" },
  },
} as Meta;

type Args = React.ComponentProps<typeof SSNEntry>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <SSNEntry {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  ssn: "",
  error: null,
  screenIndex: 2,
  screenCount: 7,
};
