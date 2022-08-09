/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import LoginForm from "./LoginForm";

export default {
  title: "Screens/Login/LoginForm",
  component: LoginForm,
  argTypes: {
    login: {
      action: "login",
    },
    goBack: {
      action: "goBack",
    },
    gotoPasswordForgot: {
      action: "gotoPasswordForgot",
    },
  },
} as Meta;

type Args = React.ComponentProps<typeof LoginForm>;

export const BaseState: Story<Args> = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <LoginForm {...args} />
  </ThemeProvider>
);

BaseState.args = {
  emailError: "None",
  passwordError: "None",
  inProgress: false,
};
