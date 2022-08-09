/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/require-await */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";
import { ThemeProvider } from "../../Theming/ThemeContext";

import GroupSplit from "./GroupSplit";

export default {
  title: "Screens/Group/GroupSplit",
  component: GroupSplit,
  argTypes: {
    trackEvent: { action: "trackEvent" },
  },
} as Meta;

type Args = React.ComponentProps<typeof GroupSplit>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <ThemeProvider>
    <View style={[StyleSheet.absoluteFill]}>
      <SafeAreaProvider>
        <GroupSplit {...args} />
      </SafeAreaProvider>
    </View>
  </ThemeProvider>
);

BaseState.args = {
  partnerInitial: "M",
  navigation: { type: "none" },
  screenIndex: 4,
  screenCount: 9,
};
