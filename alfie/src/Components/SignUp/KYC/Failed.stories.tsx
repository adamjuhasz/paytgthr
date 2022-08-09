/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Failed from "./Failed";

export default {
  title: "Screens/SignUp/KYC/Failed",
  component: Failed,
  argTypes: {},
} as Meta;

export const BaseState: Story<
  React.ComponentProps<never>
> = (): JSX.Element => (
  <ThemeProvider>
    <Failed />
  </ThemeProvider>
);
