/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Name from "./Name";

export default {
  title: "Screens/SignUp/Name",
  component: Name,
  argTypes: {
    submit: { action: "submit" },
    trackEvent: { action: "trackEvent" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof Name>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <Name {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  screenIndex: 2,
  screenCount: 7,
};

export const FilledNames: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View
      style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}
    >
      <SafeAreaProvider>
        <Name {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

FilledNames.args = {
  inProgress: false,
  firstName: "Mark",
  lastName: "Bezos",
  screenIndex: 2,
  screenCount: 7,
};
