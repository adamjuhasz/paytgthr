import React from "react";
import { ThemeProvider } from "../Theming/ThemeContext";
import { Meta, Story } from "@storybook/react/types-6-0";
import { SafeAreaProvider } from "react-native-safe-area-context";

import NotChecking from "./NotChecking";

export default {
  title: "Screens/Intro/NotChecking",
  component: NotChecking,
  argTypes: {
    next: { action: "next" },
  },
} as Meta;

type Args = React.ComponentProps<typeof NotChecking>;

export const Base: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <SafeAreaProvider>
      <NotChecking {...args} />
    </SafeAreaProvider>
  </ThemeProvider>
);

Base.args = {
  pageIndex: 0,
  pageCount: 4,
};
