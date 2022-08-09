/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import SignUpForm from "./SignUpForm";

export default {
  title: "Screens/Login/SignUpForm",
  component: SignUpForm,
  argTypes: {
    signup: {
      action: "signup",
    },
    goBack: {
      action: "goBack",
    },
  },
} as Meta;

type Args = React.ComponentProps<typeof SignUpForm>;

export const BaseState: Story<Args> = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <SignUpForm {...args} />
  </ThemeProvider>
);

BaseState.args = {
  emailError: "None",
  passwordError: "None",
  inProgress: false,
  screenIndex: 0,
  screenCount: 7,
};
