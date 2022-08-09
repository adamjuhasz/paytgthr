/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import ViewInviteCode from "./ViewInviteCode";

export default {
  title: "Screens/ViewInviteCode",
  component: ViewInviteCode,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof ViewInviteCode>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <ViewInviteCode {...args} />
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

BaseState.args = {
  code: "XYZ-123",
};
