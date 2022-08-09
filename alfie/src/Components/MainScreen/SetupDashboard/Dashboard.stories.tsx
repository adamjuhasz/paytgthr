/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import BottomBar from "../../BottomBar/BottomBar";
import Dashboard from "./Dashboard";

import LightningBolt from "../../Icons/LightningBolt";
import CreditCard from "../../Icons/CreditCard";
import Gear from "../../Icons/Gear";
import Bank from "../../Icons/Bank";

export default {
  title: "Screens/MainScreen/SetupDashboard",
  component: Dashboard,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof Dashboard>;

const pages = [
  { text: "Home", icon: LightningBolt, selected: true, action: () => 1 },
  { text: "Purchases", icon: CreditCard, selected: false, action: () => 1 },
  { text: "Payments", icon: Bank, selected: false, action: () => 1 },
  { text: "Settings", icon: Gear, selected: false, action: () => 1 },
];

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Dashboard {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

BaseState.args = {
  hasSeenIntro: false,
  hasVerifiedIdentity: false,
  hasInvitedPartner: false,
  partnerAccepted: false,
  hasVerifiedBank: false,
  haslinkedBank: false,
};
