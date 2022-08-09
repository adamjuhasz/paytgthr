/* eslint-disable @typescript-eslint/require-await */
/* eslint-disable @typescript-eslint/explicit-module-boundary-types */

import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";

import Beam from "./Beam";

export default {
  title: "Components/Avatar/Beam",
  component: Beam,
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof Beam>> = ({
  ...args
}): JSX.Element => (
  <Beam
    {...args}
  />
);

BaseState.args = {
  name: "John",
  colors: ["#EA4352", "#FCE54D", "#818CD4", "#10CCBC", "#5C6CD6"],
  size: 40,
}
