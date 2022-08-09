/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import PasswordForgot from "./PasswordForgot";

export default {
  title: "Screens/Password/Forgot",
  component: PasswordForgot,
  argTypes: {
    gotoEmailCode: { action: "gotoEmailCode" },
    gotoSMSCode: { action: "gotoSMSCode" },
    goBack: { action: "goBack" },
  },
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof PasswordForgot>> = ({
  ...args
}) => (
  <ThemeProvider>
    <PasswordForgot {...args} />
  </ThemeProvider>
);
