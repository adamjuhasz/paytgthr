/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../../Theming/ThemeContext";

import FundingSourceList from "./FundingSourceList";

export default {
  title: "Screens/Settings/FundingSourceList",
  component: FundingSourceList,
  argTypes: {
    goBack: { action: "goBack" },
    gotoVerify: { action: "gotoVerify" },
  },
} as Meta;

type Args = React.ComponentProps<typeof FundingSourceList>;

export const BaseState: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <FundingSourceList {...args} />
  </ThemeProvider>
);
BaseState.args = {
  accounts: [
    {
      accountName: "Free Checking",
      bankName: "Wells Fargo",
      verified: true,
    },
    {
      accountName: "Free Checking",
      bankName: "Wells Fargo",
      verified: false,
    },
  ],
};
