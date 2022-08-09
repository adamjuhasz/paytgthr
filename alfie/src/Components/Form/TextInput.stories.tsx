/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";
import FullScreenAutoBackground from "../Theming/AutoBackground";

import TextInput from "./TextInput";

export default {
  title: "Components/TextInput",
  component: TextInput,

  argTypes: {
    placeholder: {
      type: { name: "string", required: false },
      control: {
        type: "text",
      },
    },
    value: {
      type: { name: "string", required: false },
      control: {
        type: "text",
      },
    },
  },
} as Meta;

type Args = React.ComponentProps<typeof TextInput>;

export const BaseState = ({ ...args }: Args): JSX.Element => (
  <ThemeProvider>
    <FullScreenAutoBackground>
      <TextInput {...args} />
    </FullScreenAutoBackground>
  </ThemeProvider>
);
BaseState.args = { placeholder: "placeholder", value: undefined };
