import React from "react";
import { ThemeProvider } from "../Theming/ThemeContext";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import Pricing from "./Pricing";

export default {
  title: "Screens/Intro/Pricing",
  component: Pricing,
  argTypes: {
    next: { action: "next" },
  },
} as Meta;

type Args = React.ComponentProps<typeof Pricing>;

export const Base: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <Pricing {...args} />
    </SafeAreaProvider>
  </ThemeProvider>
);

Base.args = {
  pageIndex: 0,
  pageCount: 4,
};
