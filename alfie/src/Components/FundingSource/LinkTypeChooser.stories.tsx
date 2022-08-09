import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../Theming/ThemeContext";

import LinkTypeChooser from "./LinkTypeChooser";

export default {
  title: "Screens/Settings/FS/LinkTypeChooser",
  component: LinkTypeChooser,
  argTypes: {
    chooseManual: { action: "chooseManual" },
    goBack: { action: "goBack" },
    choosePlaid: { action: "choosePlaid" },
  },
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof LinkTypeChooser>> = ({
  ...args
}): JSX.Element => (
  <ThemeProvider>
    <LinkTypeChooser {...args} />
  </ThemeProvider>
);
