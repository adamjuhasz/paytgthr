/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { View } from "react-native";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import AllCardsLocked from "./AllCardsLocked";

export default {
  title: "Screens/MainScreen/Popups/AllCardsLocked",
  component: AllCardsLocked,
  argTypes: {
    primaryAction: { action: "primaryAction" },
    trackSeen: { action: "trackSeen" },
    trackIgnored: { action: "trackIgnored" },
  },
} as Meta;

type Args = React.ComponentProps<typeof AllCardsLocked>;

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={{ position: "absolute", top: 0, left: 0, right: 0, bottom: 0 }}>
    <SafeAreaProvider>
      <AllCardsLocked {...args} />
    </SafeAreaProvider>
  </View>
);

BaseState.args = {};
