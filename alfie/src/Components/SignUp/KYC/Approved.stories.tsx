/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";

import KYCApproved from "./Approved";

export default {
  title: "Screens/SignUp/KYC/Approved",
  component: KYCApproved,
  argTypes: {
    gotoNext: { action: "gotoNext" },
  },
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof KYCApproved>> = ({
  ...args
}): JSX.Element => (
  <ThemeProvider>
    <KYCApproved {...args} />
  </ThemeProvider>
);
