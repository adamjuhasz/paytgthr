/* eslint-disable react-native/no-inline-styles */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";
import { StyleSheet, View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import BottomBar from "../../BottomBar/BottomBar";
import Payments from "./Payments";

import CreditCard from "../../Icons/CreditCard";
import Gear from "../../Icons/Gear";
import Bank from "../../Icons/Bank";

export default {
  title: "Screens/MainScreen/Payments",
  component: Payments,
  argTypes: {},
} as Meta;

type Args = React.ComponentProps<typeof Payments>;

const pages = [
  { text: "Purchases", icon: CreditCard, selected: false, action: () => 1 },
  { text: "Payments", icon: Bank, selected: true, action: () => 1 },
  { text: "Settings", icon: Gear, selected: false, action: () => 1 },
];

export const BaseState: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Payments {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

BaseState.args = {
  credits: [],
  pendingPurchases: 123.1,
  upcomingPayments: [],
  payments: [
    {
      achinfo: ["031101279", "000000009921"],
      amount: 9500 / 100,
      id: "00000000-0000-0000-0000-000000000000",
      method: [],
      methodid:
        "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
      msgsource: "00000000-0000-0000-0000-000000000000",
      revision: 2,
      status: {
        tag: "PaymentPending",
      },
      subtype: "NormalPayment",
      text: "Tgthr Card",
      type: "DebitFromUser",
      user: "00000000-0000-0000-0000-000000000000",
      visible: true,
      createdat: new Date("2021-07-22T21:59:07.197096Z"),
    },
    {
      achinfo: ["031101279", "000000009921"],
      amount: 9500 / 100,
      id: "6e2e8864-acd7-4da6-881c-83921611111",
      method: [],
      methodid:
        "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
      msgsource: "00000000-0000-0000-0000-000000000000",
      revision: 2,
      status: {
        tag: "PaymentCompleted",
      },
      subtype: "NormalPayment",
      text: "Tgthr Card",
      type: "DebitFromUser",
      user: "00000000-0000-0000-0000-000000000000",
      visible: true,
      createdat: new Date("2021-07-22T21:59:07.197096Z"),
    },
    {
      achinfo: ["031101279", "000000009921"],
      amount: 9500 / 100,
      id: "6e2e8864-acd7-4da6-881c-83921611111",
      method: [],
      methodid:
        "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
      msgsource: "00000000-0000-0000-0000-000000000000",
      revision: 2,
      status: {
        tag: "PaymentFailed",
        contents: {
          tag: "ARCHR01",
        },
      },
      subtype: "NormalPayment",
      text: "Tgthr Card",
      type: "DebitFromUser",
      user: "00000000-0000-0000-0000-000000000000",
      visible: true,
      createdat: new Date("2021-07-22T21:59:07.197096Z"),
    },
    {
      achinfo: ["031101279", "000000009921"],
      amount: 9500 / 100,
      id: "6e2e8864-acd7-4da6-881c-83921611111",
      method: [],
      methodid:
        "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
      msgsource: "00000000-0000-0000-0000-000000000000",
      revision: 2,
      status: {
        tag: "PaymentFailed",
        contents: {
          tag: "ARCHR01",
        },
      },
      subtype: "NormalPayment",
      text: "Tgthr Card",
      type: "CreditToUser",
      user: "00000000-0000-0000-0000-000000000000",
      visible: true,
      createdat: new Date("2021-07-22T21:59:07.197096Z"),
    },
  ],
};

export const UpcomingPayment: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Payments {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

UpcomingPayment.args = {
  credits: [],
  pendingPurchases: 0,
  upcomingPayments: [{ amount: -12.43, date: new Date() }],
  payments: [
    {
      achinfo: ["031101279", "000000009921"],
      amount: 9500 / 100,
      id: "00000000-0000-0000-0000-000000000000",
      method: [],
      methodid:
        "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
      msgsource: "00000000-0000-0000-0000-000000000000",
      revision: 2,
      status: {
        tag: "PaymentPending",
      },
      subtype: "NormalPayment",
      text: "Tgthr Card",
      type: "DebitFromUser",
      user: "00000000-0000-0000-0000-000000000000",
      visible: true,
      createdat: new Date("2021-07-22T21:59:07.197096Z"),
    },
  ],
};

export const HasCredit: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Payments {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

HasCredit.args = {
  credits: [{ amount: 20 }],
  pendingPurchases: 0,
  upcomingPayments: [],
  payments: [
    {
      achinfo: ["031101279", "000000009921"],
      amount: 9500 / 100,
      id: "00000000-0000-0000-0000-000000000000",
      method: [],
      methodid:
        "https://api.dwolla.com/transfers/00000000-0000-0000-0000-000000000000",
      msgsource: "00000000-0000-0000-0000-000000000000",
      revision: 2,
      status: {
        tag: "PaymentPending",
      },
      subtype: "NormalPayment",
      text: "Tgthr Card",
      type: "DebitFromUser",
      user: "00000000-0000-0000-0000-000000000000",
      visible: true,
      createdat: new Date("2021-07-22T21:59:07.197096Z"),
    },
  ],
};

export const GhostData: Story<Args> = ({ ...args }): JSX.Element => (
  <View style={[StyleSheet.absoluteFill]}>
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar pages={pages}>
          <Payments {...args} />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

GhostData.args = {
  credits: [],
  pendingPurchases: 0,
  upcomingPayments: [],
  payments: [],
};
