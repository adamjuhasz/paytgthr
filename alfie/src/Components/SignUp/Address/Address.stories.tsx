/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { StyleSheet, View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Address from "./Address";

export default {
  title: "Screens/SignUp/Address",
  component: Address,
  argTypes: {
    submit: { action: "submit" },
    setShowHelp: { action: "setShowHelp" },
    trackEvent: { action: "trackEvent" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof Address>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => {
  return (
    <ThemeProvider>
      <View style={[StyleSheet.absoluteFill]}>
        <SafeAreaProvider>
          <Address {...args} />
        </SafeAreaProvider>
      </View>
    </ThemeProvider>
  );
};

BaseState.args = {
  inProgress: false,
  errors: { street: "None", city: "None", state: "None", zip: "None" },
  showHelp: false,
  screenIndex: 4,
  screenCount: 7,
};
