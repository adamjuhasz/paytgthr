/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import PhoneEntry from "./PhoneEntry";

export default {
  title: "Screens/SignUp/PhoneEntry",
  component: PhoneEntry,
  argTypes: {
    submit: { action: "submit" },
    trackEvent: { action: "trackEvent" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof PhoneEntry>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <PhoneEntry {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  phone: "",
  screenIndex: 2,
  screenCount: 7,
};
