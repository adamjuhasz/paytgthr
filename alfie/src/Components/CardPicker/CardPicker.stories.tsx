/* eslint-disable react-native/no-inline-styles */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import { View } from "react-native";
import { SafeAreaProvider } from "react-native-safe-area-context";

import CardPicker from "./CardPicker";

export default {
  title: "Components/CardPicker",
  component: CardPicker,
  argTypes: {},
} as Meta;

export const Stacked: Story<
  React.ComponentProps<typeof CardPicker>
> = (): JSX.Element => (
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
        <CardPicker />
      </ThemeProvider>
    </SafeAreaProvider>
  </View>
);
