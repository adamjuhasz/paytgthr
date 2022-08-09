import React from "react";
import { Meta } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import ToastDisplay from "./ToastDisplay";

export default {
  title: "Components/ToastDisplay",
  component: ToastDisplay,
  parameters: {
    backgrounds: {
      default: "gray",
      values: [{ name: "gray", value: "#7C8696" }],
    },
  },
} as Meta;

export const BaseState = (): JSX.Element => (
  <SafeAreaProvider>
    <ToastDisplay toastStr="Copied to clipboard" toastId="123" />
  </SafeAreaProvider>
);
