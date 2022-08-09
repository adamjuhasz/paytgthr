import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../../../Theming/ThemeContext";

import { ManualLinkWaiting } from "./ManalLinkWaiting";

export default {
  title: "Screens/Settings/FS/ManualLinkWaiting",
  component: ManualLinkWaiting,
  argTypes: {
    nextScreen: { action: "nextScreen" },
  },
} as Meta;

export const BaseState: Story<
  React.ComponentProps<typeof ManualLinkWaiting>
> = ({ ...args }) => (
  <ThemeProvider>
    <ManualLinkWaiting {...args} />
  </ThemeProvider>
);
