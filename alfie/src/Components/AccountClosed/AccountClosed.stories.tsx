import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import AccountClosed from "./AccountClosed";

export default {
  title: "Screens/AccountClosed",
  component: AccountClosed,
} as Meta;

export const BaseState: Story<
  React.ComponentProps<typeof AccountClosed>
> = (): JSX.Element => (
  <ThemeProvider>
    <AccountClosed />
  </ThemeProvider>
);
