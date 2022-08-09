/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import LegalConsent from "./LegalConsent";

export default {
  title: "Screens/SignUp/LegalConsent",
  component: LegalConsent,
  argTypes: {
    submitConsent: { action: "submit" },
    wontAccept: { action: "wontAccept" },
    trackEvent: { action: "trackEvent" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof LegalConsent>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <LegalConsent {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  screenIndex: 2,
  screenCount: 7,
};
