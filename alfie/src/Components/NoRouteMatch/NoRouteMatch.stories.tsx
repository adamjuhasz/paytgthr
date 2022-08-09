import React from "react";
import { Meta, Story } from "@storybook/react/types-6-0";

import NoRouteMatch from "./NoRouteMatch";

export default {
  title: "Screens/NoRouteMatch",
  component: NoRouteMatch,
  argTypes: {
    action: { action: "fix glitch" },
  },
} as Meta;

export const BaseState: Story<React.ComponentProps<typeof NoRouteMatch>> = ({
  ...args
}): JSX.Element => <NoRouteMatch {...args} />;
