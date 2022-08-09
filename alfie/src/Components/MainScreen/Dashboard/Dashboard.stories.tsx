/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";
import { View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import BottomBar from "../../BottomBar/BottomBar";
import Dashboard from "./Dashboard";

import CreditCard from "../../Icons/CreditCard";
import Gear from "../../Icons/Gear";

export default {
  title: "Screens/MainScreen/Dashboard",
  component: Dashboard,
  argTypes: {
    waitlistStash: { action: "waitlistStash" },
    waitlistSave: { action: "waitlistSave" },
    gotoPurhcase: { action: "gotoPurhcase" },
    gotoPayment: { action: "gotoPayment" },
  },
} as Meta;

type Args = React.ComponentProps<typeof Dashboard>;

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
          <Dashboard {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

BaseState.args = {
  purchases: [
    {
      id: "123",
      amount: "60.04",
      mccEmoji: "&#x1F951;",
      purchasedById: "00000000-0000-0000-0000-000000000000",
      userShare: "30.02",
      partnerRatio: "50",
      purchasedAt: "May 29",
      isDeclined: false,
      isPending: true,
      userRatio: "50",
      description: "CAL-MART SUPERMARKET",
    },
    {
      id: "234",
      amount: "60.04",
      mccEmoji: "&#x1F951;",
      purchasedById: "00000000-0000-0000-0000-000000000000",
      userShare: "30.02",
      partnerRatio: "50",
      purchasedAt: "May 29",
      isDeclined: false,
      isPending: true,
      userRatio: "50",
      description: "CAL-MART SUPERMARKET",
    },
    {
      id: "345",
      amount: "60.04",
      mccEmoji: "&#x1F951;",
      purchasedById: "00000000-0000-0000-0000-000000000000",
      userShare: "30.02",
      partnerRatio: "50",
      purchasedAt: "May 29",
      isDeclined: false,
      isPending: true,
      userRatio: "50",
      description: "CAL-MART SUPERMARKET",
    },
  ],
  stashGoals: [
    {
      sentence: "a space flight",
      name: "SpaceX flight ($250,000)",
      emoji: "🚀",
      progress: 1,
    },
    {
      sentence: "your anniversary dinner",
      name: "Anniversary date night ($650)",
      emoji: "🥂",
      progress: 40,
    },
    {
      sentence: "an adventure",
      name: "Rock climbing road trip ($800)",
      emoji: "🧗‍♂️",
      progress: 80,
    },
  ],
  fsBanners: [],
  partnerBanners: [],
};

export const NeedsInvite: Story<Args> = ({ ...args }): JSX.Element => (
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
          <Dashboard {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

NeedsInvite.args = {
  noPurchasesYet: true,
  purchases: [],
  stashGoals: [
    {
      sentence: "a space flight",
      name: "SpaceX flight ($250,000)",
      emoji: "🚀",
      progress: 1,
    },
    {
      sentence: "your anniversary dinner",
      name: "Anniversary date night ($650)",
      emoji: "🥂",
      progress: 40,
    },
    {
      sentence: "an adventure",
      name: "Rock climbing road trip ($800)",
      emoji: "🧗‍♂️",
      progress: 80,
    },
  ],
  fsBanners: [],
  partnerBanners: [],
};

export const WaitingOnInvite: Story<Args> = ({ ...args }): JSX.Element => (
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
          <Dashboard {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

WaitingOnInvite.args = {
  noPurchasesYet: true,
  purchases: [],
  stashGoals: [
    {
      sentence: "a space flight",
      name: "SpaceX flight ($250,000)",
      emoji: "🚀",
      progress: 1,
    },
    {
      sentence: "your anniversary dinner",
      name: "Anniversary date night ($650)",
      emoji: "🥂",
      progress: 40,
    },
    {
      sentence: "an adventure",
      name: "Rock climbing road trip ($800)",
      emoji: "🧗‍♂️",
      progress: 80,
    },
  ],
  fsBanners: [],
  partnerBanners: [],
};
