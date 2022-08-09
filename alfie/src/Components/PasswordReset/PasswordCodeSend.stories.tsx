/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import PasswordCodeSend from "./PasswordCodeSend";

export default {
  title: "Screens/Password/CodeSend",
  component: PasswordCodeSend,
  argTypes: {
    submit: { action: "submit" },
    goBack: { action: "goBack" },
  },
} as Meta;

export const BaseState: Story<
  React.ComponentProps<typeof PasswordCodeSend>
> = ({ ...args }) => (
  <ThemeProvider>
    <PasswordCodeSend {...args} />
  </ThemeProvider>
);

BaseState.args = {
  medium: "sms",
  inProgress: false,
};
