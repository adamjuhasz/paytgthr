/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../../../Theming/ThemeContext";
import PopUpProvider from "../../../../PopUp";

import { VerifyManualLink } from "./VerifyManualLink";

export default {
  title: "Screens/Settings/FS/VerifyManualLink",
  component: VerifyManualLink,
  argTypes: {
    submit: { action: "submit" },
    goBack: { action: "goBack" },
  },
} as Meta;

type Args = React.ComponentProps<typeof VerifyManualLink>;

export const BaseState: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <PopUpProvider>
      <VerifyManualLink {...args} />
    </PopUpProvider>
  </ThemeProvider>
);

BaseState.args = {
  bankName: "Bank of A",
  accountName: "Free Cheking",
  inProgress: false,
  amountError: "None",
};

export const MaxAttempts: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <VerifyManualLink {...args} />
  </ThemeProvider>
);

MaxAttempts.args = {
  bankName: "Bank of A",
  accountName: "Free Cheking",
  inProgress: false,
  amountError: "Attempts",
};

export const BadAmount: Story<Args> = ({ ...args }) => (
  <ThemeProvider>
    <VerifyManualLink {...args} />
  </ThemeProvider>
);

BadAmount.args = {
  bankName: "Bank of A",
  accountName: "Free Cheking",
  inProgress: false,
  amountError: "Amount",
};
