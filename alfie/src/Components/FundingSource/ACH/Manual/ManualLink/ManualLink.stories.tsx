import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../../../Theming/ThemeContext";

import { ManualLink } from "./ManualLink";

export default {
  title: "Screens/Settings/FS/ManualLink",
  component: ManualLink,
  argTypes: {
    submit: { action: "submit" },
    goBack: { action: "goBack" },
  },
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof ManualLink>> = ({
  ...args
}) => (
  <ThemeProvider>
    <ManualLink {...args} />
  </ThemeProvider>
);

BaseState.args = {
  inProgress: false,
  errors: { metaError: "None" },
  routingList: { "0": "test" },
};

export const TooManyAttempts: Story<
  React.ComponentProps<typeof ManualLink>
> = ({ ...args }) => (
  <ThemeProvider>
    <ManualLink {...args} />
  </ThemeProvider>
);

TooManyAttempts.args = {
  inProgress: false,
  errors: { metaError: "Attempts" },
  routingList: { "0": "test" },
};
