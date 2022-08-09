/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import PasswordReset from "./PasswordReset";

export default {
  title: "Screens/Password/Reset",
  component: PasswordReset,
  argTypes: {
    submit: { action: "submit" },
    goBack: { action: "goBack" },
  },
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof PasswordReset>> = ({
  ...args
}) => (
  <ThemeProvider>
    <PasswordReset {...args} />
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  passwordError: false,
  tokenFailure: false,
  tokenExpired: false,
};
