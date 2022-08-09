/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Delay from "./Delay";

export default {
  title: "Screens/SignUp/KYC/Delay",
  component: Delay,
  argTypes: {},
} as Meta;

export const BaseState: Story<
  React.ComponentProps<never>
> = (): JSX.Element => (
  <ThemeProvider>
    <Delay />
  </ThemeProvider>
);
