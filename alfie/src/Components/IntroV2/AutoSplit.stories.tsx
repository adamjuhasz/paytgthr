import React from "react";
import { ThemeProvider } from "../Theming/ThemeContext";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import AutoSplit from "./AutoSplit";

export default {
  title: "Screens/Intro/AutoSplit",
  component: AutoSplit,
  argTypes: {
    next: { action: "next" },
  },
} as Meta;

type Args = React.ComponentProps<typeof AutoSplit>;

export const Base: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <AutoSplit {...args} />
    </SafeAreaProvider>
  </ThemeProvider>
);

Base.args = {
  pageIndex: 0,
  pageCount: 4,
};
