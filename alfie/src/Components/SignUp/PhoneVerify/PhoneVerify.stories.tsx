/* eslint-disable react-native/no-inline-styles */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import PhoneVerify from "./PhoneVerify";

export default {
  title: "Screens/SignUp/PhoneVerify",
  component: PhoneVerify,
  argTypes: {
    submit: { action: "submit" },
    gotoChangePhone: { action: "gotoChangePhone" },
    sendCode: { action: "sendCode" },
    trackEvent: { action: "trackEvent" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof PhoneVerify>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
      testID="EmailVerify BaseState"
    >
      <SafeAreaProvider>
        <PhoneVerify {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  phone: "9492853343",
  error: false,
  showHelp: true,
  screenIndex: 2,
  screenCount: 7,
};

export const CodeIsWrong: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <PhoneVerify {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

CodeIsWrong.args = {
  inProgress: false,
  phone: "9492853343",
  error: true,
  showHelp: false,
  screenIndex: 2,
  screenCount: 7,
};
