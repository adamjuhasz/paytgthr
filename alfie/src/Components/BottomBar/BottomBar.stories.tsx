/* eslint-disable react-native/no-color-literals */
/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import { View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import CreditCard from "../Icons/CreditCard";
import Gear from "../Icons/Gear";
import Globe from "../Icons/Globe";
import Bank from "../Icons/Bank";

import BottomBar from "./BottomBar";

export default {
  title: "Components/BottomBar",
  component: BottomBar,
} as Meta;

type Args = React.ComponentProps<typeof BottomBar>;

export const TwoIcon: Story<Args> = ({ ...args }): JSX.Element => (
  <View
    style={{
      position: "absolute",
      top: 0,
      left: 0,
      bottom: 0,
      right: 0,
      backgroundColor: "#FF0000",
    }}
  >
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar {...args}>
          <View
            style={{
              backgroundColor: "#7C8696",
              width: "100%",
              height: "100%",
              borderColor: "#FDEE50",
              borderWidth: 1,
            }}
          />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

TwoIcon.args = {
  pages: [
    { text: "Purchases", icon: CreditCard, selected: true, action: () => 1 },
    { text: "Settings", icon: Gear, selected: false, action: () => 1 },
  ],
};

export const FourIcon: Story<Args> = ({ ...args }): JSX.Element => (
  <View
    style={{
      position: "absolute",
      top: 0,
      left: 0,
      bottom: 0,
      right: 0,
      backgroundColor: "#FF0000",
    }}
  >
    <SafeAreaProvider>
      <ThemeProvider>
        <BottomBar {...args}>
          <View
            style={{
              backgroundColor: "#7C8696",
              width: "100%",
              height: "100%",
              borderColor: "#FDEE50",
              borderWidth: 1,
            }}
          />
        </BottomBar>
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);

FourIcon.args = {
  pages: [
    { text: "Home", icon: Globe, selected: false, action: () => 1 },
    { text: "Purchases", icon: CreditCard, selected: true, action: () => 1 },
    { text: "Payments", icon: Bank, selected: false, action: () => 1 },
    { text: "Settings", icon: Gear, selected: false, action: () => 1 },
  ],
};
