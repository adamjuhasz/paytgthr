/* eslint-disable @typescript-eslint/explicit-module-boundary-types */
import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";
import { ThemeProvider } from "../../Theming/ThemeContext";

import Processing from "./Processing";

export default {
  title: "Screens/SignUp/KYC/Processing",
  component: Processing,
  argTypes: {},
} as Meta;

export const BaseState: Story<
  React.ComponentProps<never>
> = (): JSX.Element => (
  <ThemeProvider>
    <Processing />
  </ThemeProvider>
);
