/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Manual from "./Manual";

export default {
  title: "Screens/SignUp/KYC/Manual",
  component: Manual,
  argTypes: {},
} as Meta;

export const BaseState: Story<
  React.ComponentProps<never>
> = (): JSX.Element => (
  <ThemeProvider>
    <Manual />
  </ThemeProvider>
);
