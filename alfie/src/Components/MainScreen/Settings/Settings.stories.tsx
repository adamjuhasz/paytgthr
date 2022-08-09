/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";
import { View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import BottomBar from "../../BottomBar/BottomBar";
import Settings from "./Settings";

import CreditCard from "../../Icons/CreditCard";
import Gear from "../../Icons/Gear";

export default {
  title: "Screens/MainScreen/Settings",
  component: Settings,
  argTypes: {
    changeGroup: { action: "changeGroup" },
    changeFS: { action: "changeFS" },
    smsCS: { action: "smsCS" },
    callCS: { action: "callCS" },
    emailCS: { action: "emailCS" },
    requestStatement: { action: "requestStatement" },
    logout: { action: "logout" },
    orderPhysical: { action: "orderPhysical" },
  },
} as Meta;

type Args = React.ComponentProps<typeof Settings>;

const pages = [
  { text: "Purchases", icon: CreditCard, selected: false, action: () => 1 },
  { text: "Settings", icon: Gear, selected: true, action: () => 1 },
];

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <View
    style={{
      position: "absolute",
      top: 0,
      left: 0,
      bottom: 0,
      right: 0,
    }}
  >
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Settings {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

BaseState.args = {
  cards: [],
};

export const NoChangeSplit: Story<Args> = ({ ...args }): JSX.Element => (
  <View
    style={{
      position: "absolute",
      top: 0,
      left: 0,
      bottom: 0,
      right: 0,
    }}
  >
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Settings {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

NoChangeSplit.args = {
  cards: [],
  changeSplit: () => undefined,
};
