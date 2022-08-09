/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { StyleSheet, View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import DOBEntry from "./DOBEntry";

export default {
  title: "Screens/SignUp/DOBEntry",
  component: DOBEntry,
  argTypes: {
    submit: { action: "submit" },
    setShowHelp: { action: "setShowHelp" },
    trackEvent: { action: "trackEvent" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof DOBEntry>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View style={[StyleSheet.absoluteFill]}>
      <SafeAreaProvider>
        <DOBEntry {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  dob: "",
  error: null,
  showHelp: false,
  screenIndex: 5,
  screenCount: 7,
};
