import React from "react";
import { ThemeProvider } from "../Theming/ThemeContext";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import GrowingLimit from "./GrowingLimit";

export default {
  title: "Screens/Intro/GrowingLimit",
  component: GrowingLimit,
  argTypes: {
    next: { action: "next" },
  },
} as Meta;

type Args = React.ComponentProps<typeof GrowingLimit>;

export const Base: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <GrowingLimit {...args} />
    </SafeAreaProvider>
  </ThemeProvider>
);

Base.args = {
  pageIndex: 0,
  pageCount: 4,
};
