/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import FullScreenAutoBackground from "../Theming/AutoBackground";

import Label from "./Label";

export default {
  title: "Components/Label",
  component: Label,
  argTypes: {
    text: {
      type: { name: "string", required: false },
      control: {
        type: "text",
      },
    },
  },
} as Meta;

type Args = React.ComponentProps<typeof Label> & { text: string };

export const BaseState: Story<Args> = ({ text, ...args }: Args) => (
  <ThemeProvider>
    <FullScreenAutoBackground>
      <Label {...args}>{text}</Label>
    </FullScreenAutoBackground>
  </ThemeProvider>
);

BaseState.args = {
  text: "Email",
};
